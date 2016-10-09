
#include <deque>
#include <map>
#include <math.h>
#include <portaudio.h>
#include "audio.h"

using namespace std;

/* These message queues are for communication with the interpreter
   thread. */

/* For receiving commands from interpreter. */
AudioCommandQueue AudioCommands;

/* For sending replies to TIMELEFT commands back to the interpreter. */
AudioResponseQueue AudioResponses;

/* Every voice has a queue of AudioGenerator's associated with it.
   voices maps each voice number to its queue.
   Queues are deleted from the voices map when they are empty, so
   every queue in the map should always have at least one generator
   in it. */
typedef deque<AudioGenerator> GeneratorDeque;
typedef map<int, GeneratorDeque> GeneratorMap;
GeneratorMap voices;

#define SAMPLE_RATE         (44100)
// #define FRAMES_PER_BUFFER   (735)
// #define FRAMES_PER_BUFFER   (50)
#define FRAMES_PER_BUFFER   (100)

#ifndef M_PI
#define M_PI  (3.14159265)
#endif

static PaError open_stream(PaStream **streamp) {
    /* Open the audio output device. */
    PaStreamParameters outputParameters;
    PaStream *stream;
    PaError err;

    err = Pa_Initialize();
    if( err != paNoError ) return err;

    outputParameters.device = Pa_GetDefaultOutputDevice();
    if (outputParameters.device == paNoDevice) {
      fprintf(stderr,"Error: No default output device.\n");
      return err;
    }
    outputParameters.channelCount = 2;       /* stereo output */

    /* 32 bit floating point output */
    outputParameters.sampleFormat = paFloat32;

    // Pa_GetDeviceInfo( outputParameters.device )->defaultLowOutputLatency;
    outputParameters.suggestedLatency = 0.050;

    outputParameters.hostApiSpecificStreamInfo = NULL;
    err = Pa_OpenStream(
              &stream,
              NULL, /* no input */
              &outputParameters,
              SAMPLE_RATE,
              FRAMES_PER_BUFFER,
              paClipOff,      /* we won't output out of range samples so don't bother clipping them */
              NULL, /* no callback, use blocking API */
              NULL ); /* no callback, so no callback userData */
    *streamp = stream;
    return err;
}

wxThread::ExitCode AudioThread::Entry() {
    wxMessageQueueError queueres;
    AudioCommand command;
    GeneratorMap::iterator gmit;
    double time = 0;

    PaStream *stream = NULL;
    PaError err;
    bool stream_open = false;


    /* Here is the main audio loop.
       TestDestroy() will tell us when the GUI thread is asking us
       to quit.

       There are two parts to this loop:
            Handle commands
            Generate sound
     */
    while(!TestDestroy()) {

        /* Handle commands from the interpreter thread. */
        /* If we are not generating sound, then we can 
           block for a while (1/60th of a second) while we
           poll.  If we are generating sound, we should just
           check for commands and move on if we have none. */
		if(voices.empty())
            queueres = AudioCommands.ReceiveTimeout(16, command);
        else
            queueres = AudioCommands.ReceiveTimeout(1, command);

        if(queueres == wxMSGQUEUE_NO_ERROR) {
            /* We got a command. */
            switch(command.command) {
                case AudioCommand::TIMELEFT:
                    /* To return the amount of time left on a voice,
                       we need to add up all of the remaining time for
                       its generators. */
                    gmit = voices.find(command.voice);
                    if(gmit == voices.end()) {
                        /* map::find() returns map::end() if the map does
                           not contain the key.
                           If we are here, there are no generators for
                           the given voice.  Send back 0. */
                        AudioResponses.Post(0);
                    } else {
                        /* Loop over the generators in the queue for
                           the given voice and add up their times. */
                        double res = 0;
                        GeneratorDeque::iterator gdit = gmit->second.begin();
                        while(gdit != gmit->second.end()) {
                            res += gdit->duration;
                            gdit++;
                        }
                        AudioResponses.Post(res);
                    }
                    break;

                case AudioCommand::TOOTREPLACE:
                    /* Stop current sounds on the given voice and
                       fall through on purpose to add the new generator.
                       This is the same logic as in TOOTEND below. */
                    gmit = voices.find(command.voice);
                    if(gmit != voices.end()) {
                        AudioGenerator &ag = gmit->second.front();
                        if(ag.release < ag.duration)
                            ag.duration = ag.release;
                        while(gmit->second.size() > 1)
                            gmit->second.pop_back();
                    }
                    /* Fall through on purpose. */
                case AudioCommand::TOOT:
                    if(!stream) {
                        err = open_stream(&stream);
                        if( err != paNoError ) goto error;
                    }
                    /* Add the new generator to the end of the queue
                       for the given voice. */
                    if(command.generator.duration <= 0)
                        break;
                    command.generator.begin = time;
                    voices[command.voice].push_back(command.generator);
                    if(!stream_open) {
                        err = Pa_StartStream( stream );
                        if( err != paNoError ) goto error;
                        stream_open = true;
                    }
                    break;

                case AudioCommand::TOOTEND:
                    /* End the sound on a given voice.
                       We do this by truncating the first
                       generator's time left to be its
                       release time, and deleting all other
                       generators. 
                       Preserving the release prevents clicks
                       when we TOOTEND. */
                    gmit = voices.find(command.voice);
                    if(gmit != voices.end()) {
                        AudioGenerator &ag = gmit->second.front();
                        if(ag.release < ag.duration)
                            ag.duration = ag.release;
                        while(gmit->second.size() > 1)
                            gmit->second.pop_back();
                    }
                    break;

                case AudioCommand::TOOTCLEAR:
                    /* Stop all sound and stop the output stream. */
                    voices.clear();
                    if(voices.empty() && stream_open) {
                        err = Pa_StopStream( stream );
                        if( err != paNoError ) goto error;
                        stream_open = false;
                    }
                    break;
            }
        }

        /* Generate sound. */
        /* We need to fill a buffer with samples and send it to portaudio.
           For each sample, we need to calculate the current output from
           every active voice. */
        if(!voices.empty()) {
            float buffer[FRAMES_PER_BUFFER][2]; /* stereo output buffer */
			double tmptime = time;
            for(int i = 0; i < FRAMES_PER_BUFFER; i++) {
                /* This is the loop over each frame in the buffer.
                   During one pass through this loop, we will calculate
                   one left and one right audio sample. */
				buffer[i][0] = buffer[i][1] = 0;
				for(gmit = voices.begin(); gmit != voices.end();) {
                    /* To calculate the sample, we will need to add
                       up the outputs of each generator.
                       The strange use of "next" to increment gmit
                       each time through the loop is so that we can
                       erase the map entry pointed to by gmit if its
                       queue has emptied out. */
                    GeneratorMap::iterator next = gmit;
                    next++;
                    AudioGenerator &ag = gmit->second.front();

                    /* Calculate the raw sin wave sample. */
                    float amplitude = ag.volume*
                                      sin((tmptime-ag.begin)*2*M_PI*ag.frequency);
                    /* Scale it back appropriately if we are near the
                       beginning (attack) or end (release) of the sound. */
                    if(tmptime - ag.begin < ag.attack)
                        amplitude *= (tmptime-ag.begin) / ag.attack;
                    if(ag.duration < ag.release)
                        amplitude *= ag.duration/ag.release;

                    /* Add the sample into the left and right channels.
                       pan should be between -1 and 1.
                       -1 means all of the sound is on the left
                        0 means the sound is evenly balanced
                        1 means all of the sound is on the right */
                    buffer[i][0] += amplitude*(1-ag.pan);
                    buffer[i][1] += amplitude*(1+ag.pan);

                    /* Decrement the time left for this generator,
                       and remove it from its queue if there is no
                       time left. */
                    ag.duration -= 1.0/SAMPLE_RATE;
                    if(ag.duration <= 0) {
                        gmit->second.pop_front();

                        /* If the queue for the current voice is empty,
                           then we delete the current voice from the
                           voices map.  Here is the erase() which is
                           the reason for the use of "next". */
                        if(gmit->second.empty())
                            voices.erase(gmit);
                        else
                            gmit->second.front().begin = tmptime;
                    }

                    gmit = next;
                }
				tmptime += 1.0/SAMPLE_RATE;
            }
            /* Write out the buffer full of samples. */
            err = Pa_WriteStream( stream, buffer, FRAMES_PER_BUFFER );
            if( err != paNoError ) goto error;

        }

        /* Stop the output stream if there are no active generators
           for any voice. */
        if(voices.empty() && stream_open) {
            err = Pa_StopStream( stream );
            if( err != paNoError ) goto error;
            stream_open = false;
        }

		time += ((double)FRAMES_PER_BUFFER)/((double)SAMPLE_RATE);
	}

    /* We get here when the GUI thread has asked us to terminate,
       so we close the output stream and terminate. */
    if(stream) {
        err = Pa_CloseStream( stream );
        if( err != paNoError ) goto error;
        Pa_Terminate();
    }
    
    return (wxThread::ExitCode)0;

error:
    fprintf( stderr, "An error occured while using the portaudio stream\n" );
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
        // Print more information about the error.
        if( err == paUnanticipatedHostError )
        {
                const PaHostErrorInfo *hostErrorInfo = Pa_GetLastHostErrorInfo();
                fprintf( stderr, "Host API error = #%ld, hostApiType = %d\n", hostErrorInfo->errorCode, hostErrorInfo->hostApiType );
                fprintf( stderr, "Host API error = %s\n", hostErrorInfo->errorText );
        }
    Pa_Terminate();
    return (wxThread::ExitCode)0;
}

