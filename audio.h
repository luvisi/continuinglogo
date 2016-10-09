#ifndef AUDIO_H
#define AUDIO_H

#include <wx/wxprec.h>
#ifndef WX_PRECOMP
    #include <wx/wx.h>
#endif

#include <wx/msgqueue.h>


/* An AudioGenerator can be thought of as a source of sound.
   It will generate:
       a given frequency
       for a given duration
       at a given volume
       with a given pan (left/right balance),
       Ramping up to full volume during a given attack period
       Ramping down to silence, at the end, during a given release period

   Every voice has its own queue of AudioGenerator's.
   The GUI adds AudioGenerators to voice queues by sending them to
   the audio thread in a wxMessageQueue (below).
 */
class AudioGenerator {
public:
    AudioGenerator(double frequency = 0,  /* Frequency in Hertz */
                   double duration = 0,   /* Duration in seconds */
                   double volume = 1,     /* 0 <= volume <= 1 */
                   double pan = 0,        /* pan/balance
                                             -1 = all left,
                                              1 = all right,
                                              0 = balanced */
                   double attack = 0,     /* Period over which volume
                                             will ramp up at the beginning,
                                             in seconds. */
                   double release = 0) :  /* Period over which volume
                                             will ramp down at the end,
                                             in seconds. */
                   frequency(frequency),
                   duration(duration),
                   volume(volume),
                   pan(pan) ,
                   attack(attack),
                   release(release),
                   begin(0) {}
    double frequency;
    double duration;
    double volume, pan;
    double attack, release;
    double begin; /* Set when the sound starts playing. */
};

/* The interpreter thread controls the audio thread by sending
   AudioCommand's.
   TIMELEFT    sends back the time left for a given voice.
   TOOT        adds a generator to the queue for a given voice.
   TOOTREPLACE clears the queue for an existing generator and
               adds the given generator.
   TOOTEND     stops a voice.
   TOOTCLEAR   stops all voices.
 */
class AudioCommand {
public:
    enum command { TIMELEFT, TOOT, TOOTREPLACE, TOOTEND, TOOTCLEAR };
    AudioCommand(enum command command, int voice, AudioGenerator generator) :
        command(command), voice(voice), generator(generator) {}

    AudioCommand() {}

    enum command command;
	int voice;
    AudioGenerator generator;
};

typedef wxMessageQueue<AudioCommand> AudioCommandQueue;
typedef wxMessageQueue<double> AudioResponseQueue;

/* The interpreter thread sends AudioCommand commands to the audio thread
   in the AudioCommands message queue. */
extern AudioCommandQueue AudioCommands;

/* The audio thread replies to TIMELEFT commands by sending its
   answer back to the interpreter thread in the AudioResponses
   message queue. */
extern AudioResponseQueue AudioResponses;

class AudioThread : public wxThread
{
public:
    AudioThread() : wxThread(wxTHREAD_JOINABLE) { }
protected:
    virtual ExitCode Entry();
};



#endif
