
#include <deque>
#include <map>
#include <math.h>
#include <portaudio.h>
#include "audio.h"
#include "main.h"
#include "interpreter.h"
#include "ffft/FFTReal.h"

/* For locking device opens/closes and queries */
wxMutex audioLocker;

using namespace std;

/* These message queues are for communication with the interpreter
   thread. */

/* For receiving commands from interpreter. */
AudioCommandQueue AudioCommands;

/* For sending replies to TIMELEFT commands back to the interpreter. */
AudioResponseQueue AudioResponses;

/* For receiving commands from interpreter. */
RecordingCommandQueue RecordingCommands;

/* For sending replies back to the interpreter. */
RecordingResponseQueue RecordingResponses;

/* For sending device lists back to the interpreter. */
AudioDeviceListQueue AudioDeviceLists;

/* For reporting detected DTMF values back to the interpreter. */
DTMFValueQueue DTMFValues;

/* For sending back frequency power percentages. */
FrequencyPowerQueue FrequencyPowers;

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

static int pa_initialized_count = 0;

static PaError initialize_pa() {
    PaError err = paNoError;

    if(audioLocker.Lock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not lock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    if(pa_initialized_count == 0)
        err = Pa_Initialize();
    pa_initialized_count++;

    if(audioLocker.Unlock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not unlock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    return err;
}

static void terminate_pa() {
    if(audioLocker.Lock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not lock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    pa_initialized_count--;
    if(pa_initialized_count == 0)
        Pa_Terminate();

    if(audioLocker.Unlock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not unlock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }
}


static PaError open_output_stream(PaStream **streamp) {
    /* Open the audio output device. */
    PaStreamParameters outputParameters;
    PaStream *stream;
    PaError err;

    err = initialize_pa();
    if( err != paNoError ) goto end;

    if(audioLocker.Lock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not lock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    outputParameters.device = Pa_GetDefaultOutputDevice();
    if (outputParameters.device == paNoDevice) {
      fprintf(stderr,"Error: No default output device.\n");
      goto end;
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

    end:
    if(audioLocker.Unlock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not unlock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }


    return err;
}

static PaError open_input_stream(PaStream **streamp, int device, int rate, int frames) {
    /* Open the audio output device. */
    PaStreamParameters inputParameters;
    PaStream *stream;
    PaError err;

    err = initialize_pa();
    if( err != paNoError ) goto end;

    if(audioLocker.Lock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not lock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    inputParameters.device = device;
    inputParameters.channelCount = 1;       /* mono output */

    /* 32 bit floating point input */
    inputParameters.sampleFormat = paFloat32;

    inputParameters.suggestedLatency = 
        Pa_GetDeviceInfo(inputParameters.device)->defaultLowInputLatency;

    inputParameters.hostApiSpecificStreamInfo = NULL;
    err = Pa_OpenStream(
              &stream,
              &inputParameters,
              NULL, /* no output */
              rate,
              frames,
              paClipOff,      /* we won't output out of range samples so don't bother clipping them */
              NULL, /* no callback, use blocking API */
              NULL ); /* no callback, so no callback userData */
    *streamp = stream;

    end:
    if(audioLocker.Unlock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not unlock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }


    return err;
}

static PaError close_stream(PaStream *stream) {
    PaError err = paNoError;

    if(audioLocker.Lock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not lock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    if(stream) {
        err = Pa_CloseStream( stream );
    }

    if(audioLocker.Unlock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not unlock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    if(stream) {
        terminate_pa();
    }

    return err;
}

static PaError start_stream(PaStream *stream) {
    PaError err;

    if(audioLocker.Lock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not lock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    err = Pa_StartStream( stream );

    if(audioLocker.Unlock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not unlock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    return err;
}

static PaError stop_stream(PaStream *stream) {
    PaError err;

    if(audioLocker.Lock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not lock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    err = Pa_StopStream( stream );

    if(audioLocker.Unlock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not unlock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    return err;
}

wxThread::ExitCode AudioThread::Entry() {
    wxMessageQueueError queueres;
    AudioCommand command;
    GeneratorMap::iterator gmit;
    double time = 0;

    PaStream *stream = NULL;
    PaError err;
    bool stream_started = false;

    extern IC *ic; /* Terrible hack to work around circular #include's */

    ic->g->thread_start();
    void (*thread_end)();
    thread_end = ic->g->thread_end;

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
                        err = open_output_stream(&stream);
                        if( err != paNoError ) goto error;
                    }
                    /* Add the new generator to the end of the queue
                       for the given voice. */
                    if(command.generator.duration <= 0)
                        break;
                    command.generator.begin = time;
                    voices[command.voice].push_back(command.generator);
                    if(!stream_started) {
                        err = start_stream( stream );
                        if( err != paNoError ) goto error;
                        stream_started = true;
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
                    if(voices.empty() && stream_started) {
                        err = stop_stream( stream );
                        if( err != paNoError ) goto error;
                        stream_started = false;
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
        if(voices.empty() && stream_started) {
            err = stop_stream( stream );
            if( err != paNoError ) goto error;
            stream_started = false;
        }

		time += ((double)FRAMES_PER_BUFFER)/((double)SAMPLE_RATE);
	}

    /* We get here when the GUI thread has asked us to terminate,
       so we close the output stream and terminate. */
    err = close_stream(stream);
    if( err != paNoError ) goto error;
    
    thread_end();
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

void send_devices() {
    AudioDeviceList response;


    int     i, numDevices;
    const   PaDeviceInfo *deviceInfo;
    PaError err;

    err = initialize_pa();
    if( err != paNoError ) {
        printf( "ERROR: Pa_Initialize returned 0x%x\n", err );
        goto error;
    }

    if(audioLocker.Lock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not lock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    numDevices = Pa_GetDeviceCount();
    if( numDevices < 0 ) {
        printf( "ERROR: Pa_GetDeviceCount returned 0x%x\n", numDevices );
        goto error;
    }
    
    for( i=0; i<numDevices; i++ )
    {
        deviceInfo = Pa_GetDeviceInfo( i );
		response.push_back(AudioDeviceInfo(wxString(deviceInfo->name),
                                           deviceInfo->maxInputChannels,
                                           deviceInfo->maxOutputChannels,
                                           deviceInfo->defaultSampleRate));
    }

    if(audioLocker.Unlock() != wxMUTEX_NO_ERROR) {
        fprintf(stderr, "Could not unlock audioLocker mutex.\n");
        exit(EXIT_FAILURE);
    }

    AudioDeviceLists.Post(response);

    terminate_pa();
	return;

error:
    Pa_Terminate();
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
	exit(EXIT_FAILURE);
}

double magnitude_squared(float *frequency_buf, int length, int bin) {
    if(bin == 0) return frequency_buf[0]*frequency_buf[0];
    if(bin == length/2) return frequency_buf[length/2]*frequency_buf[length/2];
    return frequency_buf[bin]*frequency_buf[bin] +
           frequency_buf[length/2+bin]*frequency_buf[length/2+bin];
}

float hamming(int i, int M) {
    return 0.54 - 0.46*cos(2.0*M_PI*i/M);
}

float blackman(int i, int M) {
    return 0.42 - 0.5*cos(2.0*M_PI*i/M)+0.08*cos(4.0*M_PI*i/M);
}

void calculate_window(float (*window_function)(int, int), float *buf, int len) {
    for(int i = 0; i < len-1; i++)
        buf[i] = window_function(i, len-1);
    buf[len-1] = 0;
}

void inline calculate_powers(const ffft::FFTReal <float> *fftp,
                      float *sample_buf,
                      float *frequency_buf,
                      float *window_buf,
                      int sample_rate,
                      int frames_per_buffer,
                      const FrequencyRangeList &frequency_ranges,
                      FrequencyPowerList &result) {
    double total_msq = 0;

    for(int i = 0; i < frames_per_buffer; i++)
        sample_buf[i] *= window_buf[i];

    fftp->do_fft(frequency_buf, sample_buf);
    for(int bin = 0; bin <= frames_per_buffer/2; bin++)
        total_msq += magnitude_squared(frequency_buf, frames_per_buffer, bin);

    for(FrequencyRangeList::const_iterator i = frequency_ranges.begin();
        i != frequency_ranges.end();
        i++) {
        double f_msq = 0;
        double lowf = i->frequency - i->variation;
        int lowbin = floor(lowf/(((double)sample_rate)/((double)frames_per_buffer)));
        double highf = i->frequency + i->variation;
        int highbin = ceil(highf/(((double)sample_rate)/((double)frames_per_buffer)));
        for(int bin = lowbin; bin <= highbin; bin++)
            f_msq += magnitude_squared(frequency_buf, frames_per_buffer, bin);
        result.push_back(f_msq/total_msq);
    }
}

void discard_log_message(const char *log) {}

wxThread::ExitCode RecordingThread::Entry() {
    ffft::FFTReal <float> *fftp = NULL;

    wxMessageQueueError queueres;
    RecordingCommand command;

    float *sample_buf = NULL, *frequency_buf = NULL, *window_buf = NULL;

    float dtmf_high_threshold = 0;
    float dtmf_low_threshold = 0;
    int old_dtmf = 0, current_dtmf = 0, new_dtmf = 0;
    int dtmf_times_seen = 0, dtmf_min_seen = 1;
    FrequencyRangeList dtmf_ranges;
    dtmf_ranges.push_back(FrequencyRange(697, 10));
    dtmf_ranges.push_back(FrequencyRange(770, 10));
    dtmf_ranges.push_back(FrequencyRange(852, 10));
    dtmf_ranges.push_back(FrequencyRange(941, 10));
    dtmf_ranges.push_back(FrequencyRange(1209, 10));
    dtmf_ranges.push_back(FrequencyRange(1336, 10));
    dtmf_ranges.push_back(FrequencyRange(1477, 10));
    dtmf_ranges.push_back(FrequencyRange(1633, 10));
    dtmf_ranges.push_back(FrequencyRange(819, 222));
    dtmf_ranges.push_back(FrequencyRange(1421, 312));

    struct { int character; int rowpos; int colpos; }
        dtmf_items[] = {
            '1', 0, 4,
            '2', 0, 5,
            '3', 0, 6,
            'A', 0, 7,
            '4', 1, 4,
            '5', 1, 5,
            '6', 1, 6,
            'B', 1, 7,
            '7', 2, 4,
            '8', 2, 5,
            '9', 2, 6,
            'C', 2, 7,
            '*', 3, 4,
            '0', 3, 5,
            '#', 3, 6,
            'D', 3, 7,
            0, 0, 0 };

    double sample_rate = 0;
    int frames_per_buffer = 0;

    PaStream *stream = NULL;
    PaError err;
    bool stream_started = false;

    extern IC *ic; /* Terrible hack to work around circular #include's */

    ic->g->thread_start();
    void (*thread_end)();
    thread_end = ic->g->thread_end;

    /* Here is the main recording loop.
       TestDestroy() will tell us when the GUI thread is asking us
       to quit.

       There are two parts to this loop:
            Handle commands
            Capture and analyze samples
     */
    while(!TestDestroy()) {

        /* Handle commands from the interpreter thread. */
        /* If we are not recording, then we can 
           block for a while (1/60th of a second) while we
           poll.  If we are recording, we should just
           check for commands and move on if we have none. */
		if(!stream_started)
            queueres = RecordingCommands.ReceiveTimeout(16, command);
        else
            queueres = RecordingCommands.ReceiveTimeout(1, command);

        if(queueres == wxMSGQUEUE_NO_ERROR) {
            /* We got a command. */
            switch(command.command) {
                case RecordingCommand::QUERY_DEVICES:
                    send_devices();
                    break;
                case RecordingCommand::START:
                    sample_rate = command.sample_rate;
                    frames_per_buffer = command.samples;
                    if(!stream) {
                        err = open_input_stream(&stream,
                                                command.device,
                                                sample_rate,
                                                frames_per_buffer);
                        if(err != paNoError) {
                            printf("ERROR: open returned 0x%x\n", err);
                            goto error;
                        }
                    }
                    if(!stream_started) {
                        err = start_stream(stream);
                        if(err != paNoError) goto error;
                        stream_started = true;
                    }

                    if(sample_buf) { free(sample_buf); sample_buf = NULL; }
                    sample_buf = (float *)malloc(sizeof(float)*frames_per_buffer);
                    if(!sample_buf) {
                        fprintf(stderr, "Out of memory!\n");
                        exit(EXIT_FAILURE);
                    }

                    if(frequency_buf) { free(frequency_buf); frequency_buf = NULL; }
                    frequency_buf = (float *)malloc(sizeof(float)*frames_per_buffer);
                    if(!frequency_buf) {
                        fprintf(stderr, "Out of memory!\n");
                        exit(EXIT_FAILURE);
                    }

                    if(window_buf) { free(window_buf); window_buf = NULL; }
                    window_buf = (float *)malloc(sizeof(float)*frames_per_buffer);
                    if(!window_buf) {
                        fprintf(stderr, "Out of memory!\n");
                        exit(EXIT_FAILURE);
                    }

                    calculate_window(blackman, window_buf, frames_per_buffer);


                    if(fftp) { delete fftp; fftp = NULL; }
                    fftp = new ffft::FFTReal<float>(frames_per_buffer);
                    break;
                case RecordingCommand::START_DTMF:
                    dtmf_high_threshold = command.dtmf_high_threshold;
                    dtmf_low_threshold = command.dtmf_low_threshold;
                    break;
                case RecordingCommand::QUERY_FREQUENCIES: {
                    FrequencyPowerList result;
                    calculate_powers(fftp,
                                     sample_buf,
                                     frequency_buf,
                                     window_buf,
                                     sample_rate,
                                     frames_per_buffer,
                                     command.frequency_ranges,
                                     result);
                    FrequencyPowers.Post(result);
                    break;
                }
                case RecordingCommand::STOP:
                    if(stream_started) {
                        err = stop_stream(stream);
                        if(err != paNoError) goto error;
                        stream_started = false;
                    }
                    if(sample_buf) { free(sample_buf); sample_buf = NULL; }
                    if(frequency_buf) { free(frequency_buf); frequency_buf = NULL; }
                    if(window_buf) { free(window_buf); window_buf = NULL; }
                    if(fftp) { delete fftp; fftp = NULL; }
                    dtmf_high_threshold = 0;
                    dtmf_low_threshold = 0;
                    break;
//                case RecordingCommand::SILENCE_LOGGING:
//                    PaUtil_SetDebugPrintFunction(discard_log_message);
//                    break;
            }
        }

        /* Capture samples */
        if(stream_started) {
            err = Pa_ReadStream(stream, sample_buf, frames_per_buffer);
            if(err != paNoError) goto error;

            if(dtmf_high_threshold != 0) {
                /* If we're decoding DTMF tones, see if there's something
                   we need to send back to the interpreter. */



                FrequencyPowerList dtmf_powers;
                calculate_powers(fftp,
                                 sample_buf,
                                 frequency_buf,
                                 window_buf,
                                 sample_rate,
                                 frames_per_buffer,
                                 dtmf_ranges,
                                 dtmf_powers);
                float rowpower = dtmf_powers[8];
                float colpower = dtmf_powers[9];
                for(int i = 0; i <= 3; i++)
                    dtmf_powers[i] /= rowpower;
                for(int i = 4; i <= 7; i++)
                    dtmf_powers[i] /= colpower;

//                for(int i = 0; i < 8; i++)
//                    fprintf(stderr, "%7.2f", dtmf_powers[i]);
//                fprintf(stderr, "\n");

                bool dtmf_defined = true;
                for(int i = 0; i < 8; i++)
                    if(dtmf_powers[i] > dtmf_low_threshold &&
                       dtmf_powers[i] < dtmf_high_threshold) {
                        dtmf_defined = false;
                        break;
                    }

                int dtmf_row = -1;
                if(dtmf_defined)
                    for(int i = 0; i < 4; i++)
                        if(dtmf_powers[i] >= dtmf_high_threshold) {
                            if(dtmf_row == -1) {
                                dtmf_row = i;
                            } else {
                                dtmf_row = -1;
                                dtmf_defined = false;
                                break;
                            }
                        }

                int dtmf_column = -1;
                if(dtmf_defined)
                    for(int i = 4; i < 8; i++)
                        if(dtmf_powers[i] >= dtmf_high_threshold) {
                            if(dtmf_column == -1) {
                                dtmf_column = i;
                            } else {
                                dtmf_column = -1;
                                dtmf_defined = false;
                                break;
                            }
                        }

                if((dtmf_row == -1) != (dtmf_column == -1))
                    dtmf_defined = false;

                if(dtmf_defined) {
                    new_dtmf = 0;
                    if(dtmf_row != -1 && dtmf_column != -1) {
                        for(int i = 0; dtmf_items[i].character; i++) {
                            if(dtmf_items[i].rowpos == dtmf_row &&
                               dtmf_items[i].colpos == dtmf_column) {
                                new_dtmf = dtmf_items[i].character;
                                break;
                            }
                        }
                    }

                    current_dtmf = new_dtmf;
                    if(current_dtmf != old_dtmf) {
                        old_dtmf = current_dtmf;
                        if(current_dtmf != 0)
                            DTMFValues.Post(current_dtmf);
                        if(current_dtmf == 0)
                            fprintf(stderr, "  : ");
                        else
                            fprintf(stderr, " %c: ", current_dtmf);
                            
                        for(int i = 0; i < 8; i++)
                            fprintf(stderr, "%7.2f", dtmf_powers[i]);
                        fprintf(stderr, "\n");
                    }
                }

/*
                if(new_dtmf == current_dtmf) {
                    dtmf_times_seen++;
                    if(dtmf_times_seen >= dtmf_min_seen) {
                        dtmf_times_seen = 0;
                        if(current_dtmf != old_dtmf) {
                            old_dtmf = current_dtmf;
                            if(current_dtmf != 0)
                                DTMFValues.Post(current_dtmf);
                        }
                    }
                } else {
                    current_dtmf = new_dtmf;
                    dtmf_times_seen = 0;
                }
*/
            }
        }
	}

    /* We get here when the GUI thread has asked us to terminate,
       so we close the output stream and terminate. */

    if(stream_started) {
        err = stop_stream(stream);
        if(err != paNoError) {
            fprintf(stderr, "Couldn't stop stream\n");
            goto error;
        }
        stream_started = false;
    }

    if(stream) {
        err = close_stream(stream);
        if(err != paNoError) {
            fprintf(stderr, "Couldn't close stream\n");
            goto error;
        }
    }
    
    if(sample_buf) { free(sample_buf); sample_buf = NULL; }
    if(frequency_buf) { free(frequency_buf); frequency_buf = NULL; }
    if(window_buf) { free(window_buf); window_buf = NULL; }
    if(fftp) { delete fftp; fftp = NULL; }

    thread_end();
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

