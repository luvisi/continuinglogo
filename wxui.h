
#ifndef WXUI_H
#define WXUI_H

#include <wx/wxprec.h>
#ifndef WX_PRECOMP
    #include <wx/wx.h>
#endif

#include <wx/msgqueue.h>
#include <wx/dcgraph.h>
#include <wx/graphics.h>
#include <wx/dcbuffer.h>



#include <iostream>

#include "main.h"
#include "audio.h"

using namespace std;

/* The background bitmap.
   All drawing and stamping occurs here. */
extern wxBitmap * volatile Background;

/* The foreground bitmap.  Used as a buffer before final display.
   During display, the background is copied to the foreground, and all
   turtles print themselves onto the foreground. */
extern wxBitmap * volatile Foreground;

/* The color of those portions of the background upon which nothing
   has been drawn, or which have been erased with PENERASE. */
extern wxColour BackgroundColour;

#include "turtles.h"

/* DrawEvent's are sent to the GUI thread by the interpreter thread
   to influence the display by drawing something or giving a turtle
   an instruction or some such.
   Lots of different constructors because different events carry
   different argyments. */
class DrawEvent : public wxEvent {
public:
    /* Used by CLEAR_SCREEN, CLEAN_SCREEN, FETCHTURTLES,
       AUTOREFRESHON, AUTOREFRESHOFF, REFRESH, BACKGROUND */
    DrawEvent(wxEventType eventType) : wxEvent(0, eventType) { }

    /* Used by CAPTUREBITMAP, CAPTURESHAPE */
    DrawEvent(wxEventType eventType,
              int turtle,
              double x1,
              double y1,
              double x2,
              double y2,
              double xcenter,
              double ycenter,
              bool transparent)
        : wxEvent(0, eventType), turtle(turtle),
          x1(x1), y1(y1), x2(x2), y2(y2), xcenter(xcenter), ycenter(ycenter),
          transparent(transparent) { }

    /* Used by SETX, SETY, SETROTATION, ROTATE, SETHEADING,
       RIGHT, FORWARD, SETPENSIZE, SETXSCALE, SETYSCALE */
    DrawEvent(wxEventType eventType,
              int turtle,
              double arg)
        : wxEvent(0, eventType), turtle(turtle),
          x1(arg), y1(arg), angle(arg), distance(arg) { }

    /* Used by SETXY, DRAW_ARC */
    DrawEvent(wxEventType eventType,
              int turtle,
              double arg1,
              double arg2)
        : wxEvent(0, eventType), turtle(turtle),
          x1(arg1), y1(arg2), angle(arg1), radius(arg2) { }

    /* Used by XCOR, YCOR, ROTATION, HEADING, DELETETURTLE, PENUP,
       PENDOWN, SHOWTURTLE, HIDETURTLE, PATHTURTLE, WRAP, WINDOW,
       FENCE, TURTLEMODE, SHOWN, PENDOWNP, PENSIZE, XSCALE, YSCALE,
       PENPAINT, PENERASE, PENCOLOR, PENMODE, FILL, STAMP, GETTEXT */
    DrawEvent(wxEventType eventType, int turtle)
        : wxEvent(0, eventType), turtle(turtle) { }

    /* Used by BITMAPTURTLE */
    DrawEvent(wxEventType eventType, int turtle, wxString s, double x, double y)
        : wxEvent(0, eventType), turtle(turtle), s(s), x1(x), y1(y) { }

    /* Used by SAVE_TURTLE, LABEL, SETTEXT */
    DrawEvent(wxEventType eventType, int turtle, wxString s)
        : wxEvent(0, eventType), turtle(turtle), s(s) { }

    /* Used by SETBG */
    DrawEvent(wxEventType eventType,
              unsigned char red, unsigned char green, unsigned char blue)
        : wxEvent(0, eventType), red(red), green(green), blue(blue) { }
    /* Used by SETPENCOLOR */
    DrawEvent(wxEventType eventType, int turtle,
              unsigned char red, unsigned char green, unsigned char blue)
        : wxEvent(0, eventType), turtle(turtle), red(red), green(green), blue(blue) { }
    virtual wxEvent *Clone() const { return new DrawEvent(*this); }
    int turtle;
    wxString s;
    double x1, y1, x2, y2, angle, radius, distance;
    double xcenter, ycenter;
    unsigned char red, green, blue;
    bool transparent;
};

/* Declare the events. */
wxDECLARE_EVENT(DRAW_LINE, DrawEvent);
wxDECLARE_EVENT(DRAW_ARC, DrawEvent);
wxDECLARE_EVENT(CLEAN_SCREEN, DrawEvent);
wxDECLARE_EVENT(CLEAR_SCREEN, DrawEvent);
wxDECLARE_EVENT(XCOR, DrawEvent);
wxDECLARE_EVENT(YCOR, DrawEvent);
wxDECLARE_EVENT(HEADING, DrawEvent);
wxDECLARE_EVENT(SETHEADING, DrawEvent);
wxDECLARE_EVENT(RIGHT, DrawEvent);
wxDECLARE_EVENT(ROTATION, DrawEvent);
wxDECLARE_EVENT(SETROTATION, DrawEvent);
wxDECLARE_EVENT(ROTATE, DrawEvent);
wxDECLARE_EVENT(FORWARD, DrawEvent);
wxDECLARE_EVENT(SETXY, DrawEvent);
wxDECLARE_EVENT(SETX, DrawEvent);
wxDECLARE_EVENT(SETY, DrawEvent);
wxDECLARE_EVENT(FETCHTURTLES, DrawEvent);
wxDECLARE_EVENT(DELETETURTLE, DrawEvent);
wxDECLARE_EVENT(PENUP, DrawEvent);
wxDECLARE_EVENT(PENDOWN, DrawEvent);
wxDECLARE_EVENT(SHOWTURTLE, DrawEvent);
wxDECLARE_EVENT(HIDETURTLE, DrawEvent);
wxDECLARE_EVENT(BITMAPTURTLE, DrawEvent);
wxDECLARE_EVENT(PATHTURTLE, DrawEvent);
wxDECLARE_EVENT(WRAP, DrawEvent);
wxDECLARE_EVENT(WINDOW, DrawEvent);
wxDECLARE_EVENT(FENCE, DrawEvent);
wxDECLARE_EVENT(TURTLEMODE, DrawEvent);
wxDECLARE_EVENT(SHOWN, DrawEvent);
wxDECLARE_EVENT(PENDOWNP, DrawEvent);
wxDECLARE_EVENT(SETBG, DrawEvent);
wxDECLARE_EVENT(SETPENCOLOR, DrawEvent);
wxDECLARE_EVENT(SETPENSIZE, DrawEvent);
wxDECLARE_EVENT(PENSIZE, DrawEvent);
wxDECLARE_EVENT(CAPTUREBITMAP, DrawEvent);
wxDECLARE_EVENT(CAPTURESHAPE, DrawEvent);
wxDECLARE_EVENT(SETSHAPE, DrawEvent);
wxDECLARE_EVENT(AUTOREFRESHON, DrawEvent);
wxDECLARE_EVENT(AUTOREFRESHOFF, DrawEvent);
wxDECLARE_EVENT(REFRESH, DrawEvent);
wxDECLARE_EVENT(SETXSCALE, DrawEvent);
wxDECLARE_EVENT(SETYSCALE, DrawEvent);
wxDECLARE_EVENT(XSCALE, DrawEvent);
wxDECLARE_EVENT(YSCALE, DrawEvent);
wxDECLARE_EVENT(SAVE_TURTLE, DrawEvent);
wxDECLARE_EVENT(PENPAINT, DrawEvent);
wxDECLARE_EVENT(PENERASE, DrawEvent);
wxDECLARE_EVENT(BACKGROUND, DrawEvent);
wxDECLARE_EVENT(PENCOLOR, DrawEvent);
wxDECLARE_EVENT(PENMODE, DrawEvent);
wxDECLARE_EVENT(FILL, DrawEvent);
wxDECLARE_EVENT(LABEL, DrawEvent);
wxDECLARE_EVENT(SETTEXT, DrawEvent);
wxDECLARE_EVENT(GETTEXT, DrawEvent);
wxDECLARE_EVENT(STAMP, DrawEvent);
wxDECLARE_EVENT(COLORUNDER, DrawEvent);
wxDECLARE_EVENT(XYCOLORUNDER, DrawEvent);


/*

The interpreter runs in a separate thread from the GUI, which allows
the interpreter to suspend within recursive procedures waiting
for input from the user.

The interpreter thread sends messages to the GUI by sending events
using QueueEvent(), which is thread safe.

The GUI sends strings to the interpreter using wxMessageQueue's of
wxString's.  The main queue, called "queue" is for normal user input.
editedQueue is for transmitting back a chunk of text edited using
a dialog.  The number queue, called numberQueue is used for sending
numbers back from the GUI to the interpreter.

*/

/* Custom events sent from the interpreter to the GUI thread. */

/* Add some text to the output text area.  */
wxDECLARE_EVENT(PRINT_TEXT, wxCommandEvent);

/* Enter character mode, where the GUI transmits each character to
   the interpreter as it is typed. */
wxDECLARE_EVENT(CHAR_MODE, wxCommandEvent);

/* Enter line mode, where the GUI waits until the user presses
   enter to transmit input to the interpreter. */
wxDECLARE_EVENT(LINE_MODE, wxCommandEvent);

/* Enter poll mode, where the GUI tracks which keys are down
   and the interpreter can poll to ask if a given key is down at
   any given moment. */
wxDECLARE_EVENT(POLL_MODE, wxCommandEvent);

/* Is a particular key down? */
wxDECLARE_EVENT(POLL_KEY, wxCommandEvent);

/* Set the prompt (shown to the left of the input field). */
wxDECLARE_EVENT(SET_PROMPT, wxCommandEvent);

/* Edit a string using a popup dialog. */
wxDECLARE_EVENT(EDIT_STRING, wxCommandEvent);

/* Move the input cursor in the popup editing dialog to the beginning
   of the text area.
   This is part of a hack.
   See ContinuingLogoApp::OnEditString() in wxui.cpp.*/
wxDECLARE_EVENT(ZERO_POSITION, wxCommandEvent);

/* Save/load the background to/from a file. */
wxDECLARE_EVENT(SAVEPICT, wxCommandEvent);
wxDECLARE_EVENT(LOADPICT, wxCommandEvent);

/* StringQueue's are used for sending strings from the GUI thread to
   the interpreter thread. */
typedef wxMessageQueue<wxString> StringQueue;

/* A NumberQueue is used for sending numbers from the GUI to
   the interpreter thread. */
typedef wxMessageQueue<double> NumberQueue;

/* A StringVector is used for tracking input history. */
typedef wxVector<wxString> StringVector;

/* IntVector's are used to collect the list of turtles who are 
   touching a given turtle or background location in the *WHOSTOUCHING
   commands.
   Then the GUI can tell the interpreter how many turtles were found
   before sending them in the NumberQueue. */
typedef wxVector<int> IntVector;

/* See CHAR_MODE, LINE_MODE, and POLL_MODE above. */
enum inputmode { CHARMODE, LINEMODE, POLLMODE };

/* The thread that we will start for running the interpreter. */
class InterpreterThread : public wxThread
{
public:
    /* We use a joinable thread for the interpreter so that the GUI 
       can wait for the interpreter to shut down before exiting the
       program. */
    InterpreterThread() : wxThread(wxTHREAD_JOINABLE) { }
protected:
    virtual ExitCode Entry();
};

/* A ContinuingLogoApp will be created by wxWidgets at program startup. */
class ContinuingLogoApp: public wxApp
{
public:
    virtual bool OnInit();
    void OnExit(wxCommandEvent& event);         /* Exit menu option. */
    void OnClose(wxCloseEvent& event);          /* Window close button. */
    void OnEnter(wxCommandEvent& event);        /* User presses enter. */
    void OnPrintText(wxCommandEvent& event);    /* Interpreter prints text. */
    void OnChar(wxKeyEvent& event);             /* User presses a key. */
    void OnKeyDown(wxKeyEvent& event);          /* Track in poll mode. */
    void OnKeyUp(wxKeyEvent& event);            /* Track in poll mode. */
    void OnCharmode(wxCommandEvent& event);     /* Enter char mode. */
    void OnLinemode(wxCommandEvent& event);     /* Enter line mode. */
    void OnPollmode(wxCommandEvent& event);     /* Enter poll mode. */
    void OnPollkey(wxCommandEvent& event);      /* Is a given key down? */
    void OnSetPrompt(wxCommandEvent& event);    /* Set the prompt. */
    void OnInterrupt(wxCommandEvent& event);    /* ^C or interrupt button. */
    void OnPause(wxCommandEvent& event);        /* ^\ or pause button. */
    void OnSave(wxCommandEvent& event);         /* Save workspace to file */
    void OnLoad(wxCommandEvent& event);         /* Load workspace from file */
    void OnEditString(wxCommandEvent& event);   /* Edit string w/popup. */
    void OnZeroPosition(wxCommandEvent& event); /* Curser to popup start. */
    void paint_canvas(wxDC &dc);                /* Utility function. */
    void OnPaintCanvas(wxPaintEvent& event);    /* Paint the graphics canvas. */
    void OnPaint(wxPaintEvent& event);          /* Paint the graphics canvas. */
    void OnDrawArc(DrawEvent& event);           /* Draw an arc on Background */
    void OnCleanScreen(DrawEvent& event);       /* Clean the background */
    void OnClearScreen(DrawEvent& event);       /* Clean background and reset
                                                   turtles. */
    void OnXcor(DrawEvent& event);              /* Fetch turtle x coordinate */
    void OnYcor(DrawEvent& event);              /* Fetch turtle y coordinate */
    void OnHeading(DrawEvent& event);           /* Fetch turtle heading */
    void OnSetHeading(DrawEvent& event);        /* Set turtle heading */
    void OnForward(DrawEvent& event);           /* Move turtle forward */
    void OnRight(DrawEvent& event);             /* Turn turtle right */
    void OnRotation(DrawEvent& event);          /* Fetch appearance rotation */
    void OnSetRotation(DrawEvent& event);       /* Set appearance rotation */
    void OnRotate(DrawEvent& event);            /* Rotate appearance */
    void OnSetxy(DrawEvent& event);             /* Set turtle x and y */
    void OnSetx(DrawEvent& event);              /* Set turtle x */
    void OnSety(DrawEvent& event);              /* Set turtle y */
    void OnFetchTurtles(DrawEvent& event);      /* Get a list of turtles */
    void OnDeleteTurtle(DrawEvent& event);      /* Delete a turtle */
    void OnPenup(DrawEvent& event);             /* Stop drawing */
    void OnPendown(DrawEvent& event);           /* Start drawing */
    void OnShowturtle(DrawEvent& event);        /* Show the turtle */
    void OnHideturtle(DrawEvent& event);        /* Hide the turtle */
    void OnBitmapTurtle(DrawEvent& event);      /* Load a bitmap for a turtle */
    void OnPathTurtle(DrawEvent& event);        /* Turtle to triangle */
    void OnWrap(DrawEvent& event);              /* Wrap at edge */
    void OnWindow(DrawEvent& event);            /* Run off edge */
    void OnFence(DrawEvent& event);             /* Stop at edge */
    void OnTurtleMode(DrawEvent& event);        /* Fetch turtle mode */
    void OnShown(DrawEvent& event);             /* Is turtle shown? */
    void OnPendownp(DrawEvent& event);          /* Is turtle drawing? */
    void OnSetbg(DrawEvent& event);             /* Set background color */
    void OnSetpencolor(DrawEvent& event);       /* Set turtle's pen color */
    void OnSetpensize(DrawEvent& event);        /* Set turtle's pen size */
    void OnPensize(DrawEvent& event);           /* Fetch turtle's pen size */
    void OnCapturebitmap(DrawEvent& event);     /* Make a turtle be a
                                                   snapshot of part of the
                                                   screen */
    void OnCaptureshape(DrawEvent& event);      /* Capture a bitmap in a
                                                   shape for later use
                                                   in a turtle. */
    void OnSetshape(DrawEvent& event);          /* Assign a shape to
                                                   a turtle. */
    void OnAutorefreshon(DrawEvent& event);     /* Redraw screen after graphics
                                                   events */
    void OnAutorefreshoff(DrawEvent& event);    /* Don't redraw after graphics
                                                   events */
    void OnRefresh(DrawEvent& event);           /* Redraw manually */
    void OnSetxscale(DrawEvent& event);         /* Scale the turtle's width */
    void OnSetyscale(DrawEvent& event);         /* Scale the turtle's height */
    void OnXscale(DrawEvent& event);            /* Fetch width scaling */
    void OnYscale(DrawEvent& event);            /* Fetch height scaling */
    void OnSaveTurtle(DrawEvent& event);        /* Save turtle image to file */
    void OnSavePict(wxCommandEvent& event);     /* Save background to file */
    void OnLoadPict(wxCommandEvent& event);     /* Load background from file */
    void OnPenPaint(DrawEvent& event);          /* Make pen draw */
    void OnPenErase(DrawEvent& event);          /* Make pen erase */
    void OnBackground(DrawEvent& event);        /* Fetch background color */
    void OnPencolor(DrawEvent& event);          /* Fetch turtle pen color */
    void OnPenmode(DrawEvent& event);           /* Fetch turtle pen mode 
                                                   (PAINT, ERASE) */
    void OnFill(DrawEvent& event);              /* Flood fill region */
    void OnLabel(DrawEvent& event);             /* Print text on canvas */
    void OnSettext(DrawEvent& event);           /* Set text shown by turtle */
    void OnGettext(DrawEvent& event);           /* Get text shown by turtle */
    void OnStamp(DrawEvent& event);             /* Stamp turtle image on
                                                   background */
    void OnColorunder(DrawEvent& event);        /* Fetch the color of the
                                                   background pixel under
                                                   the turtle. */
    void OnXyColorunder(DrawEvent& event);      /* Fetch the color of the
                                                   background pixel under
                                                   the turtle relative (x, y)
                                                   coordinate. */

    /* Is the turtle over any pixels of a given color? */
    void OnOver(CollisionTestEvent& event);

    /* Is a particular pixel in the turtle over a given color? */
    void OnXyOver(CollisionTestEvent& event);

    /* Is a turtle touching another particular turtle? */
    void OnTouching(CollisionTestEvent& event);

    /* Is a particular pixel in the turtle touching another
       particular turtle? */
    void OnXyTouching(CollisionTestEvent& event);

    /* What turtles is a given turtle touching? */
    void OnWhosTouching(CollisionTestEvent& event);

    /* What turtles is a given pixel in the turtle touching? */
    void OnXyWhosTouching(CollisionTestEvent& event);

    /* What turtles are touching a particular location on the screen? */
    void OnGlobalXyWhosTouching(CollisionTestEvent& event);

    /* Allow the interpreter thread to fetch pointers to the queues. */
    StringQueue *GetStringQueue();
    StringQueue *GetEditedQueue();
    NumberQueue *GetNumberQueue();
private:
    enum inputmode inputmode;   /* CHARMODE, LINEMODE, or POLLMODE */

    /* GUI components. */
    wxFrame *frame;            /* Top level frame */
    wxPanel *panel;            /* Container just inside top level frame */
    wxWindow *canvas;          /* Where the turtle draws */
    wxStaticText *prompt;      /* Text field to the left of the input box
                                  that can be changed by the interpreter
                                  with SET_PROMPT events */
    wxTextCtrl *input;         /* Where the user types input */
    wxTextCtrl *text;          /* Logged input and output */

    InterpreterThread *interpreterThread; /* Interpreter thread */
    AudioThread *audioThread;             /* Audio thread */
    StringQueue queue;         /* Sends typed input to interpreter thread */
    StringQueue editedQueue;   /* Sends text edited w/popup to interpreter
                                  thread */
    NumberQueue numberQueue;   /* Sends numbers back to interpreter thread */
    StringVector history;      /* Input history */
    StringVector::const_iterator iterator; /* Current history position */
    bool iterator_valid; /* Is "iterator" currently usable? */
    void ShutDown();
    bool autorefresh; /* Are we refreshing the canvas automatically?
                         Turning refresh off while doing lots of drawing
                         can speed things up. */

    /* These three maps work together to handle keyboard polling.
       keysDown contains true values for any keys which will poll
       as down the next time we get a POLL_KEY event from the interpreter.
       The added complication of keysPolled and keysLifted is to ensure
       that a key will poll as true at least once every time it is
       pressed, even if it is released before we get the POLL_KEY
       event.

       keysPolled tracks keys for which we have received a POLL_KEY event.
       keysLifted tracks keys for which we have received a wxEVT_KEY_UP.

       A key remains "down" in keysPolled until it has been both lifted
       and polled.

       The state machine looks something like this, for a given key.
       A state is a tripple, (down, polled, lifted).
       
                    wxEVT_KEY_DOWN    POLL_KEY            wxEVT_KEY_UP     
       (f, f, f)    (t, f, f)(press)  (f, t, f)		  (f, f, t)        
       (t, f, f)    (t, f, f)         (t, t, f)		  (t, f, t)        
       (t, f, t)    (t, f, t)         (f, f, f)(reset)	  (t, f, t)        
       (t, t, f)    (t, t, f)         (t, t, f)		  (f, f, f)(reset) 
							                   
       (f, t, f)    (t, f, f)(press)  (f, t, f)		  (f, f, f)(reset) 
       (f, f, t)    (t, f, f)(press)  (f, f, f)(reset)	  (f, f, t)        
       
       (f, t, t)(impossible)
       (t, t, t)(impossible)

     */
    std::map<int, bool> keysDown;   /* Which keys are currently down. */
    std::map<int, bool> keysPolled; /* Which keys have geen polled since
                                       being pressed, and can be marked
                                       as up. */
    std::map<int, bool> keysLifted; /* Which keys have been lifted. */
};

/* This informs wxWidgets that the wxApp object for this application
   will be a ContinuingLogoApp. */
wxDECLARE_APP(ContinuingLogoApp);

/* Custon window IDs for use in generating GUI events. */
enum
{
    /* Used in events generated by the interrupt and pause buttons. */
    ID_Interrupt = 1,
    ID_Pause,
    ID_Save,
    ID_Load,
};


/* Used to let the interpreter thread wait for the GUI thread to finish
   an operation. */
extern wxMutex turtleConditionLocker;
extern wxCondition turtleCondition;

/* This is the data structure containing all active turtles.
   Every turtle is numbered with an integer.  This maps
   each turtle's number to its object. */
typedef std::map<int, Turtle> IntTurtleMap;
extern IntTurtleMap TurtleMap;

/* A Shape represents a bitmap and offsets that can be assigned to
   a turtle. */
class Shape {
    public:
    Shape(wxBitmap bm, int xoffset, int yoffset) :
        bm(bm), xoffset(xoffset), yoffset(yoffset) {}
    Shape() {}
    wxBitmap bm;
    int xoffset, yoffset;
};

/* This holds shapes that can be assigned to turtles.
   Shapes are captured with CAPTURESHAPE and assigned with SETSHAPE. */
typedef std::map<int, Shape> IntShapeMap;
extern IntShapeMap ShapeMap;

#endif
