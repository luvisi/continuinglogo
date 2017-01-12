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


/* This is for recording sound */

class FrequencyRange {
public:
    FrequencyRange(double frequency, double variation) :
        frequency(frequency), variation(variation) {}
    double frequency;
    double variation;
};

typedef wxVector<FrequencyRange> FrequencyRangeList;
typedef wxVector<double> FrequencyPowerList;
typedef wxMessageQueue<FrequencyPowerList> FrequencyPowerQueue;

extern FrequencyPowerQueue FrequencyPowers;

class RecordingCommand {
public:
    enum command { QUERY_DEVICES, START, QUERY_FREQUENCIES, STOP,
                   START_DTMF, SILENCE_LOGGING };
    RecordingCommand(enum command command) : command(command) {}
    RecordingCommand(enum command command,
                     int device,
                     int sample_rate,
                     int samples) :
        command(command),
        device(device),
        sample_rate(sample_rate),
        samples(samples) {}
    RecordingCommand(enum command command,
                     FrequencyRangeList frequency_ranges) :
        command(command),
        frequency_ranges(frequency_ranges) {}
    RecordingCommand(enum command command,
                     float dtmf_high_threshold,
                     float dtmf_low_threshold) :
        command(command),
        dtmf_high_threshold(dtmf_high_threshold),
        dtmf_low_threshold(dtmf_low_threshold) {}
    RecordingCommand() {}

    enum command command;
    int device, sample_rate, samples;
    float dtmf_high_threshold;
    float dtmf_low_threshold;
    FrequencyRangeList frequency_ranges;
};

class AudioDeviceInfo {
public:
    AudioDeviceInfo(const wxString &name,
                    int max_inputs,
                    int max_outputs,
                    double default_sample_rate) :
        name(name),
        max_inputs(max_inputs),
        max_outputs(max_outputs),
        default_sample_rate(default_sample_rate) {}

    wxString name;
    int max_inputs, max_outputs;
    double default_sample_rate;
};

typedef wxVector<AudioDeviceInfo> AudioDeviceList;

typedef wxMessageQueue<RecordingCommand> RecordingCommandQueue;
typedef wxMessageQueue<double> RecordingResponseQueue;
typedef wxMessageQueue<AudioDeviceList> AudioDeviceListQueue;
typedef wxMessageQueue<int> DTMFValueQueue;

/* The interpreter thread sends RecordingCommand commands to the audio thread
   in the RecordingCommands message queue. */
extern RecordingCommandQueue RecordingCommands;

/* The audio thread replies to TIMELEFT commands by sending its
   answer back to the interpreter thread in the RecordingResponses
   message queue. */
extern RecordingResponseQueue RecordingResponses;

extern AudioDeviceListQueue AudioDeviceLists;

extern DTMFValueQueue DTMFValues;

class RecordingThread : public wxThread
{
public:
    RecordingThread() : wxThread(wxTHREAD_JOINABLE) { }
protected:
    virtual ExitCode Entry();
};

/* Used to lock all opens/closes/queries.
   portaudio documentation says that reads/writes on different channels
   should be safe most of the time without locking. 
   I hope that it's correct. */
extern wxMutex audioLocker;

#endif
