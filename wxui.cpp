// wxWidgets "Hello world" Program
// For compilers that support precompilation, includes "wx/wx.h".

#include <iostream>
#include <stdio.h>
#include "wxui.h"
#include "interpreter.h"
#include "turtles.h"
#include <wx/filedlg.h>

/* Custom events.  See wxui.h */
wxDEFINE_EVENT(PRINT_TEXT, wxCommandEvent);
wxDEFINE_EVENT(CHAR_MODE, wxCommandEvent);
wxDEFINE_EVENT(LINE_MODE, wxCommandEvent);
wxDEFINE_EVENT(POLL_MODE, wxCommandEvent);
wxDEFINE_EVENT(POLL_KEY, wxCommandEvent);
wxDEFINE_EVENT(SET_PROMPT, wxCommandEvent);
wxDEFINE_EVENT(EDIT_STRING, wxCommandEvent);
wxDEFINE_EVENT(ZERO_POSITION, wxCommandEvent);
wxDEFINE_EVENT(SAVEPICT, wxCommandEvent);
wxDEFINE_EVENT(LOADPICT, wxCommandEvent);

wxMutex turtleConditionLocker;
wxCondition turtleCondition(turtleConditionLocker);


IntTurtleMap TurtleMap; /* Global Turtles container */
IntShapeMap ShapeMap; /* Global shapes container */
wxBitmap * volatile Background; /* Global background image */
wxBitmap * volatile Foreground; /* Shadow copy of image being displayed */
wxColour BackgroundColour(*wxBLACK);

/* See header files for explanations of these events. */
wxDEFINE_EVENT(DRAW_LINE, DrawEvent);
wxDEFINE_EVENT(DRAW_ARC, DrawEvent);
wxDEFINE_EVENT(CLEAN_SCREEN, DrawEvent);
wxDEFINE_EVENT(CLEAR_SCREEN, DrawEvent);
wxDEFINE_EVENT(XCOR, DrawEvent);
wxDEFINE_EVENT(YCOR, DrawEvent);
wxDEFINE_EVENT(HEADING, DrawEvent);
wxDEFINE_EVENT(SETHEADING, DrawEvent);
wxDEFINE_EVENT(RIGHT, DrawEvent);
wxDEFINE_EVENT(ROTATION, DrawEvent);
wxDEFINE_EVENT(SETROTATION, DrawEvent);
wxDEFINE_EVENT(ROTATE, DrawEvent);
wxDEFINE_EVENT(FORWARD, DrawEvent);
wxDEFINE_EVENT(SETXY, DrawEvent);
wxDEFINE_EVENT(SETX, DrawEvent);
wxDEFINE_EVENT(SETY, DrawEvent);
wxDEFINE_EVENT(FETCHTURTLES, DrawEvent);
wxDEFINE_EVENT(DELETETURTLE, DrawEvent);
wxDEFINE_EVENT(PENUP, DrawEvent);
wxDEFINE_EVENT(PENDOWN, DrawEvent);
wxDEFINE_EVENT(SHOWTURTLE, DrawEvent);
wxDEFINE_EVENT(HIDETURTLE, DrawEvent);
wxDEFINE_EVENT(BITMAPTURTLE, DrawEvent);
wxDEFINE_EVENT(PATHTURTLE, DrawEvent);
wxDEFINE_EVENT(WRAP, DrawEvent);
wxDEFINE_EVENT(WINDOW, DrawEvent);
wxDEFINE_EVENT(FENCE, DrawEvent);
wxDEFINE_EVENT(TURTLEMODE, DrawEvent);
wxDEFINE_EVENT(SHOWN, DrawEvent);
wxDEFINE_EVENT(PENDOWNP, DrawEvent);
wxDEFINE_EVENT(SETBG, DrawEvent);
wxDEFINE_EVENT(SETPENCOLOR, DrawEvent);
wxDEFINE_EVENT(SETPENSIZE, DrawEvent);
wxDEFINE_EVENT(PENSIZE, DrawEvent);
wxDEFINE_EVENT(CAPTUREBITMAP, DrawEvent);
wxDEFINE_EVENT(CAPTURESHAPE, DrawEvent);
wxDEFINE_EVENT(SETSHAPE, DrawEvent);
wxDEFINE_EVENT(AUTOREFRESHON, DrawEvent);
wxDEFINE_EVENT(AUTOREFRESHOFF, DrawEvent);
wxDEFINE_EVENT(REFRESH, DrawEvent);
wxDEFINE_EVENT(SETXSCALE, DrawEvent);
wxDEFINE_EVENT(SETYSCALE, DrawEvent);
wxDEFINE_EVENT(XSCALE, DrawEvent);
wxDEFINE_EVENT(YSCALE, DrawEvent);
wxDEFINE_EVENT(SAVE_TURTLE, DrawEvent);
wxDEFINE_EVENT(PENPAINT, DrawEvent);
wxDEFINE_EVENT(PENERASE, DrawEvent);
wxDEFINE_EVENT(BACKGROUND, DrawEvent);
wxDEFINE_EVENT(PENCOLOR, DrawEvent);
wxDEFINE_EVENT(PENMODE, DrawEvent);
wxDEFINE_EVENT(FILL, DrawEvent);
wxDEFINE_EVENT(LABEL, DrawEvent);
wxDEFINE_EVENT(SETTEXT, DrawEvent);
wxDEFINE_EVENT(GETTEXT, DrawEvent);
wxDEFINE_EVENT(STAMP, DrawEvent);
wxDEFINE_EVENT(COLORUNDER, DrawEvent);
wxDEFINE_EVENT(XYCOLORUNDER, DrawEvent);

wxDEFINE_EVENT(TURTLE_OVER, CollisionTestEvent);
wxDEFINE_EVENT(TURTLEXY_OVER, CollisionTestEvent);
wxDEFINE_EVENT(TURTLE_TOUCHING, CollisionTestEvent);
wxDEFINE_EVENT(TURTLEXY_TOUCHING, CollisionTestEvent);
wxDEFINE_EVENT(TURTLE_WHOSTOUCHING, CollisionTestEvent);
wxDEFINE_EVENT(TURTLEXY_WHOSTOUCHING, CollisionTestEvent);
wxDEFINE_EVENT(GLOBALXY_WHOSTOUCHING, CollisionTestEvent);

/* This tells wxWidgets that the application's main wxApp object
   will be a ContinuingLogoApp.
   We use the NO_MAIN variation so that we can still start the app
   via our own main, which fires up the GUI by calling wxEntry(). */
wxIMPLEMENT_APP_NO_MAIN(ContinuingLogoApp);

/* Set up the GUI.  main() calls wxEntry(), which creates a ContinuingLogoApp
   and calls OnInit(). */
bool ContinuingLogoApp::OnInit()
{
    /* Top level windows are called "Frames."
       Create the main frame for user interaction. */
    frame = new wxFrame(NULL,
                        wxID_ANY,
                        "ContinuingLogo",
                        wxPoint(50, 50),
                        wxSize(950, 550));

    /* Set up the menu bar. */
    wxMenu *menuFile = new wxMenu;
    menuFile->Append(wxID_EXIT);
    wxMenu *menuHelp = new wxMenu;
    menuHelp->Append(wxID_ABOUT);
    wxMenuBar *menuBar = new wxMenuBar;
    menuBar->Append(menuFile, "&File");
    menuBar->Append(menuHelp, "&Help");
    frame->SetMenuBar(menuBar);

    /* Set up the status bar (place for short message at the bottom). */
    frame->CreateStatusBar();
    frame->SetStatusText("");

    /* Event handlers for events coming from the user. */
    
    /* Exit menu option */
    frame->Bind(wxEVT_COMMAND_MENU_SELECTED, &ContinuingLogoApp::OnExit, this, wxID_EXIT);

    /* Close window button */
    frame->Bind(wxEVT_CLOSE_WINDOW, &ContinuingLogoApp::OnClose, this);

    /* Interrupt button */
    frame->Bind(wxEVT_BUTTON, &ContinuingLogoApp::OnInterrupt, this, ID_Interrupt);

    /* Pause button */
    frame->Bind(wxEVT_BUTTON, &ContinuingLogoApp::OnPause, this, ID_Pause);

    /* Save button */
    frame->Bind(wxEVT_BUTTON, &ContinuingLogoApp::OnSave, this, ID_Save);

    /* Load button */
    frame->Bind(wxEVT_BUTTON, &ContinuingLogoApp::OnLoad, this, ID_Load);



    /* Event handlers for events coming from the interpreter 
       thread. */

    /* Sent by BYE to tell the GUI to terminate. */
    Bind(wxEVT_CLOSE_WINDOW, &ContinuingLogoApp::OnClose, this);

    /* Interpreter output */
    Bind(PRINT_TEXT, &ContinuingLogoApp::OnPrintText, this);

    /* Enter char mode */
    Bind(CHAR_MODE, &ContinuingLogoApp::OnCharmode, this);

    /* Enter line mode */
    Bind(LINE_MODE, &ContinuingLogoApp::OnLinemode, this);

    /* Enter poll mode */
    Bind(POLL_MODE, &ContinuingLogoApp::OnPollmode, this);

    /* Is a given key down? */
    Bind(POLL_KEY, &ContinuingLogoApp::OnPollkey, this);

    /* Set the prompt */
    Bind(SET_PROMPT, &ContinuingLogoApp::OnSetPrompt, this);

    /* Edit string w/popup */
    Bind(EDIT_STRING, &ContinuingLogoApp::OnEditString, this);

    /* Move entry point in popup editor to the beginning. */
    Bind(ZERO_POSITION, &ContinuingLogoApp::OnZeroPosition, this); 

    /* Save a turtle as a png */
    Bind(SAVE_TURTLE, &ContinuingLogoApp::OnSaveTurtle, this); 

    /* Save/load the background */
    Bind(SAVEPICT, &ContinuingLogoApp::OnSavePict, this); 
    Bind(LOADPICT, &ContinuingLogoApp::OnLoadPict, this); 

    /* Make the turtle paint with its own color or with the
       background color. */
    Bind(PENPAINT, &ContinuingLogoApp::OnPenPaint, this); 
    Bind(PENERASE, &ContinuingLogoApp::OnPenErase, this); 

    /* Fetch the background color, the pen color, and the pen mode */
    Bind(BACKGROUND, &ContinuingLogoApp::OnBackground, this); 
    Bind(PENCOLOR, &ContinuingLogoApp::OnPencolor, this); 
    Bind(PENMODE, &ContinuingLogoApp::OnPenmode, this); 

    /* Fill a region */
    Bind(FILL, &ContinuingLogoApp::OnFill, this); 

    /* Print text on the background */
    Bind(LABEL, &ContinuingLogoApp::OnLabel, this); 

    /* Show text by turtle */
    Bind(SETTEXT, &ContinuingLogoApp::OnSettext, this); 

    /* Get text shown by turtle */
    Bind(GETTEXT, &ContinuingLogoApp::OnGettext, this); 

    /* Stamp an image of the turtle on the background */
    Bind(STAMP, &ContinuingLogoApp::OnStamp, this); 

    /* Return the color of the background pixel under the turtle. */
    Bind(COLORUNDER, &ContinuingLogoApp::OnColorunder, this); 

    /* Return the color of the background pixel under the turtle relative
       (x, y) point. */
    Bind(XYCOLORUNDER, &ContinuingLogoApp::OnXyColorunder, this); 

    /* Repaint the canvas.  Sent after turtle state changes. */
    Bind(wxEVT_PAINT, &ContinuingLogoApp::OnPaint, this); 

    /* Fetch the x coordinate of the turtle. */
    Bind(XCOR, &ContinuingLogoApp::OnXcor, this); 

    /* Fetch the y coordinate of the turtle. */
    Bind(YCOR, &ContinuingLogoApp::OnYcor, this); 

    /* Fetch the heading of the turtle. */
    Bind(HEADING, &ContinuingLogoApp::OnHeading, this); 

    /* Set the heading of the turtle. */
    Bind(SETHEADING, &ContinuingLogoApp::OnSetHeading, this); 

    /* Fetch, set, and adjust appearance rotation.
       This impacts display, but not direction of motion. */
    Bind(ROTATION, &ContinuingLogoApp::OnRotation, this); 
    Bind(SETROTATION, &ContinuingLogoApp::OnSetRotation, this); 
    Bind(ROTATE, &ContinuingLogoApp::OnRotate, this); 

    /* Draw an arc on Background on instructions from interpreter */
    Bind(DRAW_ARC, &ContinuingLogoApp::OnDrawArc, this); 

    /* Clean the canvas. This does not move any turtles. */
    Bind(CLEAN_SCREEN, &ContinuingLogoApp::OnCleanScreen, this); 

    /* Clean the canvas and move the turtles to (0, 0). */
    Bind(CLEAR_SCREEN, &ContinuingLogoApp::OnClearScreen, this); 

    /* Move a turtle forward. */
    Bind(FORWARD, &ContinuingLogoApp::OnForward, this); 

    /* Turn a turtle */
    Bind(RIGHT, &ContinuingLogoApp::OnRight, this); 

    /* Move a turtle to (x, y) */
    Bind(SETXY, &ContinuingLogoApp::OnSetxy, this); 

    /* Set a turtle's x coordinate */
    Bind(SETX, &ContinuingLogoApp::OnSetx, this); 

    /* Set a turtle's y coordinate */
    Bind(SETY, &ContinuingLogoApp::OnSety, this); 

    /* Send a list of turtles back to the interpreter. */
    Bind(FETCHTURTLES, &ContinuingLogoApp::OnFetchTurtles, this); 

    /* Delete a turtle. */
    Bind(DELETETURTLE, &ContinuingLogoApp::OnDeleteTurtle, this); 

    /* Stop drawing */
    Bind(PENUP, &ContinuingLogoApp::OnPenup, this); 

    /* Start drawing */
    Bind(PENDOWN, &ContinuingLogoApp::OnPendown, this); 

    /* Show */
    Bind(SHOWTURTLE, &ContinuingLogoApp::OnShowturtle, this); 

    /* Hide */
    Bind(HIDETURTLE, &ContinuingLogoApp::OnHideturtle, this); 

    /* Load bitmap */
    Bind(BITMAPTURTLE, &ContinuingLogoApp::OnBitmapTurtle, this); 

    /* Switch to path */
    Bind(PATHTURTLE, &ContinuingLogoApp::OnPathTurtle, this); 

    /* Enter wrap mode */
    Bind(WRAP, &ContinuingLogoApp::OnWrap, this); 

    /* Enter window mode */
    Bind(WINDOW, &ContinuingLogoApp::OnWindow, this); 

    /* Enter fence mode */
    Bind(FENCE, &ContinuingLogoApp::OnFence, this); 

    /* Fetch the turtle mode */
    Bind(TURTLEMODE, &ContinuingLogoApp::OnTurtleMode, this); 

    /* Is the turtle shown? */
    Bind(SHOWN, &ContinuingLogoApp::OnShown, this); 

    /* Is the pen down? */
    Bind(PENDOWNP, &ContinuingLogoApp::OnPendownp, this); 

    /* Set the background color */
    Bind(SETBG, &ContinuingLogoApp::OnSetbg, this); 

    /* Set the turtle's pen color */
    Bind(SETPENCOLOR, &ContinuingLogoApp::OnSetpencolor, this); 

    /* Set the turtle's pen size */
    Bind(SETPENSIZE, &ContinuingLogoApp::OnSetpensize, this); 

    /* Fetch the turtle's pen size */
    Bind(PENSIZE, &ContinuingLogoApp::OnPensize, this); 

    /* Give the turtle a bitmap captured from the screen */
    Bind(CAPTUREBITMAP, &ContinuingLogoApp::OnCapturebitmap, this); 

    /* Capture a shape from the screen */
    Bind(CAPTURESHAPE, &ContinuingLogoApp::OnCaptureshape, this); 

    /* Give the turtle a bitmap copied from a shape */
    Bind(SETSHAPE, &ContinuingLogoApp::OnSetshape, this); 

    /* Turn autorefresh on and off */
    Bind(AUTOREFRESHON, &ContinuingLogoApp::OnAutorefreshon, this); 
    Bind(AUTOREFRESHOFF, &ContinuingLogoApp::OnAutorefreshoff, this); 

    /* Force a refresh */
    Bind(REFRESH, &ContinuingLogoApp::OnRefresh, this); 

    /* Scale the turtle along its own x or y axis */
    Bind(SETXSCALE, &ContinuingLogoApp::OnSetxscale, this); 
    Bind(SETYSCALE, &ContinuingLogoApp::OnSetyscale, this); 

    /* Fetch scale factors */
    Bind(XSCALE, &ContinuingLogoApp::OnXscale, this); 
    Bind(YSCALE, &ContinuingLogoApp::OnYscale, this); 

    /* Is the turtle over any pixels matching a criteria? */
    Bind(TURTLE_OVER, &ContinuingLogoApp::OnOver, this); 

    /* Is a particular pixel (turtle relative coordinates) over
       a pixel matching a criteria? */
    Bind(TURTLEXY_OVER, &ContinuingLogoApp::OnXyOver, this); 

    /* Is a particular turtle touching another particular turtle? */
    Bind(TURTLE_TOUCHING, &ContinuingLogoApp::OnTouching, this); 

    /* Is a particular pixel (turtle relative coordinates) of a
       particular turtle touching any part of another 
       particular turtle? */
    Bind(TURTLEXY_TOUCHING, &ContinuingLogoApp::OnXyTouching, this); 

    /* What other turtles are touching a given turtle? */
    Bind(TURTLE_WHOSTOUCHING, &ContinuingLogoApp::OnWhosTouching, this); 

    /* What other turtles are touching a particular pixel (turtle
       relative coordinates) of a particular turtle? */
    Bind(TURTLEXY_WHOSTOUCHING, &ContinuingLogoApp::OnXyWhosTouching, this); 

    /* What turtles are over a particular global location? */
    Bind(GLOBALXY_WHOSTOUCHING, &ContinuingLogoApp::OnGlobalXyWhosTouching, this); 


    /* Container for controls. Docs only say it handles tabbing between
       controls, but other things break without one, so here it is. */
    panel = new wxPanel(frame);

    /* Sizers handle sizing and layout of controls.  All controls are
       a direct child of the panel in a window hierarchy sense, but
       there is a parallel sizer hierarchy that handles their layout. */

    /* Horizontal sizer that holds user interaction controls on the left
       and the turtle canvas on the right. */
    wxBoxSizer *topsizer = new wxBoxSizer(wxHORIZONTAL);
    panel->SetSizer(topsizer);

    /* Vertical sizer that contains the user interaction controls. */
    wxBoxSizer *column = new wxBoxSizer(wxVERTICAL);
    topsizer->Add(column, 1, wxALL | wxEXPAND);

    /* Text control for displaying output from the interpreter thread.
       Note the wxTE_READONLY.  The user cannot edit this.  It only gets new
       content from the interpreter. */
    text = new wxTextCtrl(panel,
                          wxID_ANY,
                          wxEmptyString,
                          wxDefaultPosition,
                          wxDefaultSize,
                          wxTE_MULTILINE | wxTE_READONLY);
    column->Add(text, 1, wxALL | wxEXPAND);

    /* inputrow holds the prompt and the input control. */
    wxBoxSizer *inputrow = new wxBoxSizer(wxHORIZONTAL);

    /* Static text field for displaying the prompt to the left of the
       input control. */
    prompt = new wxStaticText(panel,
                              wxID_ANY,
                              wxT(""));
    inputrow->Add(prompt, 0, wxALL | wxEXPAND);

    /* Input text control. Must generate and handle events when the
       user presses enter and when the user presses any key (used for
       character mode and poll mode). */
    input = new wxTextCtrl(panel,
                           wxID_ANY,
                           wxEmptyString,
                           wxDefaultPosition,
                           wxDefaultSize,
                           wxTE_PROCESS_ENTER);
    /* The OnEnter() handler is used in line mode.  It sends a line of
       text to the interpreter when the user presses enter. */
    input->Bind(wxEVT_TEXT_ENTER, &ContinuingLogoApp::OnEnter, this);

    /* The OnChar() handler is used in character mode.  It sends every
       character directly to the interpreter.  It also does interrupt
       and pause handling even in line mode. */
    input->Bind(wxEVT_CHAR, &ContinuingLogoApp::OnChar, this);

    /* OnKeyDown and OnKeyUp are used for tracking which keys are currently
       down in POLL_MODE. */
    input->Bind(wxEVT_KEY_DOWN, &ContinuingLogoApp::OnKeyDown, this);
    input->Bind(wxEVT_KEY_UP, &ContinuingLogoApp::OnKeyUp, this);
       
    inputrow->Add(input, 1, wxALL | wxEXPAND);
    column->Add(inputrow, 0, wxALL | wxEXPAND);

    /* These are the buttons that let the user interrupt or pause
       the interpreter. */
    wxBoxSizer *modebuttons = new wxBoxSizer(wxHORIZONTAL);
    modebuttons->Add(new wxButton(panel, ID_Interrupt, wxT("Interrupt")),
                     0, wxALL | wxEXPAND);
    modebuttons->Add(new wxButton(panel, ID_Pause, wxT("Pause")),
                     0, wxALL | wxEXPAND);
    modebuttons->Add(new wxButton(panel, ID_Save, wxT("Save")),
                     0, wxALL | wxEXPAND);
    modebuttons->Add(new wxButton(panel, ID_Load, wxT("Load")),
                     0, wxALL | wxEXPAND);
    column->Add(modebuttons, 0, wxALL | wxEXPAND);

    /* The canvas is where the turtles live and draw things. */
    canvas = new wxWindow(panel, wxID_ANY, wxDefaultPosition,
                          wxSize(501, 501));
    topsizer->Add(canvas, 0, wxALL);
    /* Handler for painting the canvas.  Displays Background and then
       adds the turtles. */
    canvas->Bind(wxEVT_PAINT, &ContinuingLogoApp::OnPaintCanvas, this);

    /* Start out in line mode. */
    inputmode = LINEMODE;

    /* We have no history to begin with. */
    iterator_valid = false;

    /* Bitmap upon which turtle's draw */
    Background = new wxBitmap(501, 501);

    /* Bitmap used during redisplay.  Contains background plus images of
       turtles. */
    Foreground = new wxBitmap(501, 501);

    /* Clear the Background bitmap to begin with. */
    wxMemoryDC canvasDC;
    canvasDC.SelectObject(*Background);
    canvasDC.SetBackground(wxBrush(BackgroundColour));
    canvasDC.Clear();
    canvasDC.SelectObject(wxNullBitmap);

    /* Create a turtle. */
    TurtleMap[0] = Turtle();

    /* Start the interpreter thread. */
    interpreterThread = new InterpreterThread();
    if(interpreterThread->Run() != wxTHREAD_NO_ERROR) {
        wxLogError("Can't create the interpreter thread!");
        delete interpreterThread;
        interpreterThread = NULL;
    }

    /* Start the audio thread. */
    audioThread = new AudioThread();
    if(audioThread->Run() != wxTHREAD_NO_ERROR) {
        wxLogError("Can't create the audio thread!");
        delete audioThread;
        audioThread = NULL;
    }

    /* Display the top level window and set the focus so the user can
       start typing commands. */
    frame->Show(true);
    input->SetFocus();

    /* By default, redraw the canvas every time anything changes.
       This can be turned off for performance with AUTOREFRESHOFF
       and then refreshes can be performed manually with REFRESH. */
    autorefresh = true;

    return true;
}

void ContinuingLogoApp::OnExit(wxCommandEvent& event) {
    /* Send an wxEVT_CLOSE_WINDOW event to the main frame
       so that OnClose() will be called. */
    frame->Close(true);
}

/* Handle an wxEVT_CLOSE_WINDOW event. The user is trying to
   quit the application. */
void ContinuingLogoApp::OnClose(wxCloseEvent& event) {
    /* Ask the interpreter thread to end itself and then wait for it.
       The interpreter thread regularly calls TestDestroy to find out
       if we're waiting for it to quit. */
    if(interpreterThread->Delete() != 0) {
        wxLogError("Can't delete the audio thread!");
    }
    delete interpreterThread;

    /* Ask the audio thread to end itself and then wait for it.
       The audio thread regularly calls TestDestroy to find out
       if we're waiting for it to quit. */
    if(audioThread->Delete() != 0) {
        wxLogError("Can't delete the audio thread!");
    }
    delete audioThread;

    frame->Destroy();
}

/* Switch to character mode. */
void ContinuingLogoApp::OnCharmode(wxCommandEvent& event) {
    inputmode = CHARMODE;
}

/* Switch to line mode. */
void ContinuingLogoApp::OnLinemode(wxCommandEvent& event) {
    inputmode = LINEMODE;
}

/* Switch to poll mode. */
void ContinuingLogoApp::OnPollmode(wxCommandEvent& event) {
    inputmode = POLLMODE;
}

/* Set the prompt. */
void ContinuingLogoApp::OnSetPrompt(wxCommandEvent& event) {
    prompt->SetLabel(event.GetString());
    input->SetValue("");
    input->SetFocus();
    panel->Layout();
}

/* User pressed ^C or the interrupt button. */
void ContinuingLogoApp::OnInterrupt(wxCommandEvent& event) {
    signalLocker.Lock();
    interrupted = 1;
    signalLocker.Unlock();
}

/* User pressed ^\ or the pause button. */
void ContinuingLogoApp::OnPause(wxCommandEvent& event) {
    signalLocker.Lock();
    paused = 1;
    signalLocker.Unlock();
}

/* Save workspace button was pressed */
void ContinuingLogoApp::OnSave(wxCommandEvent& event) {
    wxFileDialog saveDialog(frame,
                            _("Save ContinuingLogo Workspace"),
                            "",
                            "",
                            "ContinuingLogo Files (*.lg)|*.lg|All Files (*)|*",
                            wxFD_SAVE|wxFD_OVERWRITE_PROMPT|wxFD_CHANGE_DIR);
    if (saveDialog.ShowModal() != wxID_CANCEL) {
        wxString path = saveDialog.GetPath();

        *input << "save \"|";
        unsigned int len = path.Len();
        for(unsigned int i = 0; i < len; i++) {
            char ch = path[i];
            if(ch == '|' || ch == '\\')
                *input << '\\';
            *input << ch;
        }
        if(len < 3 || path[len-3] != '.' ||
           path[len-2] != 'l' || path[len-1] != 'g')
            *input << ".lg";
        *input << "|";
    }
    input->SetFocus();
    input->SetInsertionPoint(input->GetLastPosition());
}

/* Load workspace button was pressed */
void ContinuingLogoApp::OnLoad(wxCommandEvent& event) {
    wxFileDialog loadDialog(frame,
                            _("Load ContinuingLogo Workspace"),
                            "",
                            "",
                            "ContinuingLogo Files (*.lg)|*.lg|All Files (*)|*",
                            wxFD_OPEN|wxFD_FILE_MUST_EXIST|wxFD_CHANGE_DIR);
    if(loadDialog.ShowModal() != wxID_CANCEL) {
        wxString path = loadDialog.GetPath();

        *input << "load \"|";
        for(unsigned int i = 0; i < path.Len(); i++) {
            char ch = path[i];
            if(ch == '|' || ch == '\\')
                *input << '\\';
            *input << ch;
        }
        *input << "|";
    }

    input->SetFocus();
    input->SetInsertionPoint(input->GetLastPosition());
}

/* To allow the user to edit things interactively using EDIT.
   WXEDIT/wxedit_subr sends an EDIT_STRING event to the GUI
   including the string to edit, and then waits for the edited
   text to come back through the editedQueue. */
void ContinuingLogoApp::OnEditString(wxCommandEvent& event) {
    wxTextEntryDialog dialog(frame,
                             "",                /* Message */
                             "Editing",         /* Title */
                             event.GetString(), /* Initial contents */
                             wxOK | wxCANCEL | wxTE_MULTILINE);
    dialog.SetSize(400,400);

    /* ShowModal (below) moves the insert point to the end of
       the text area in the popup.  Therefore, to make it appear
       with the insert point at the beginning, we must call
       SetInsertionPoint(0) on the text entry control *after*
       the window has poped up.  However, our code is paused
       inside of ShowModal() running a sub event loop, so we don't
       get control back until the dialog is dismissed.

       To run our code right after the dialog pops up, we queue
       a ZERO_POSITION event for ourselves that includes a pointer
       to the text control.  This event will be processed inside of
       the ShowModal event loop *after* the dialog pops up, so it
       will move the insertion point to the beginning of the text
       area.
     */

    /* Find the text entry control */
    wxWindow *win = dialog.FindWindow(wxTextCtrlNameStr);
    wxTextCtrl *text = wxDynamicCast(win, wxTextCtrl);
    if(text) {
        /* Queue the event for moving to the beginning of the text
           entry control. */
        wxCommandEvent *zeroPosition = new wxCommandEvent(ZERO_POSITION);
        zeroPosition->SetClientData(text);
        QueueEvent(zeroPosition);
    }

    /* Show the dialog.  ShowModal() does not return until the user
       has pressed Ok or Cancel. */
    if (dialog.ShowModal() == wxID_OK) {
        /* The user pressed Ok.  Send the edited text back to
           WXEDIT/wxedit_subr to be returned to whatever Logo
           code wanted the user to edit something. */
        editedQueue.Post(dialog.GetValue().Clone() + "\n");
    } else {
        /* The user pressed Cancel.  Send back the original text. */
        editedQueue.Post(event.GetString());
    }
}

/* Move the input cursor in an edit popup to the beginning.
   See discussion in OnEditString() above. */
void ContinuingLogoApp::OnZeroPosition(wxCommandEvent& event) {
    wxTextCtrl *text = (wxTextCtrl *) event.GetClientData();
    text->SetInsertionPoint(0);
}

/* The interpreter wants to add text to the output control. */
void ContinuingLogoApp::OnPrintText(wxCommandEvent& event) {
    *text << event.GetString();
    panel->Layout();
}

/* Handle the user pressing enter in line mode. */
void ContinuingLogoApp::OnEnter(wxCommandEvent& event) {
    wxString ivalue = input->GetValue();

    /* Do nothing if we're in charmode.
       That's handled by OnChar below. */
    if(inputmode == CHARMODE)
        return;

    /* Copy the prompt and input text to the output text area.
       This is the equivalent of a terminal echoing its input. */
    *text << prompt->GetLabel() << input->GetValue() << "\n";

    /* Send a copy of the input to the interpreter thread.
       We use Clone() to ensure that no shared data structures are
       being accessed in the wxString from multiple threads. */
    queue.Post(ivalue.Clone() + '\n');

    /* Add the input to the history vector if it isn't empty
       and isn't the same as the last line we sent. */
    if(ivalue != "" &&
       (history.empty() || history.back() != ivalue)) {
        history.push_back(ivalue);
    }

    /* Invalidate the history iterator. After pressing return we are
       always at the end of the history. */
    iterator_valid = false;

    prompt->SetLabel("");
    input->SetValue("");
    panel->Layout(); /* Must relayout things every time the prompt changes. */
}

/* Process an individual character event. */
void ContinuingLogoApp::OnChar(wxKeyEvent& event) {
    wxChar uc = event.GetUnicodeKey();
    int kc = event.GetKeyCode();

    /* For keys that actually have a Unicode value, uc has that
       value.  For special keys like arrow keys, uc will 
       contain WXK_NONE and we check the value of kc. */

    if(inputmode == CHARMODE) {
        if(uc != WXK_NONE) {
            /* If we're in charmode then we just send the keystroke
               directly to the interpreter thread. */
            queue.Post(wxString((char)uc));
        } else {
            /* This tells wxWidgets to continue processing the event
               as if we hadn't caught it. */
            event.Skip();
        }
    } else {
        /* We are in line mode.  Handle a few special cases and
           otherwise call Skip() to let things be processed normally. */
        
        if(uc == '\003') {
            /* Control-C */
            signalLocker.Lock();
            interrupted = 1;
            signalLocker.Unlock();
        } else if(uc == '\134' && event.GetModifiers() == wxMOD_CONTROL) {
            /* Control-\ */
            signalLocker.Lock();
            paused = 1;
            signalLocker.Unlock();
        } else if(uc == WXK_NONE &&
                  kc == WXK_UP &&
                  event.GetModifiers() == wxMOD_NONE) {
            /* The user pressed Up. */

            /* Force the iterator to be valid.
               If the history is empty then end() will equal begin()
               and there will be nothing to do below. */
            if(!iterator_valid) {
                iterator = history.end();
                iterator_valid = true;
            }

            if(iterator != history.begin()) {
                iterator--;
                input->SetValue(*iterator);
                input->SetInsertionPointEnd();
            }
        } else if(uc == WXK_NONE &&
                  kc == WXK_DOWN &&
                  event.GetModifiers() == wxMOD_NONE) {
            /* The user pressed Down. */

            if(iterator_valid && iterator != history.end()) {
                iterator++;
                if(iterator == history.end()) {
                    input->SetValue("");
                } else {
                    input->SetValue(*iterator);
                    input->SetInsertionPointEnd();
                }
            }
        } else {
            /* Let the control handle the keystroke as normal (add
               a character, delete, left/right arrows, etc.). */
            event.Skip();
        }
    }
}


/* OnPollKey(), OnKeyDown(), and OnKeyUp() work together to implement
   keyboard polling.  The idea is to make sure that every keystroke
   will cause a poll of that key to return true at least once.
   See the comments in wxui.h for the theory of how keysDown, keysPolled,
   and keysLifted work together. */

/* Is a given key down? */
void ContinuingLogoApp::OnPollkey(wxCommandEvent& event) {
    bool ret = keysDown[event.GetInt()];
    int key = event.GetInt();
    if(keysLifted[key]) {
        keysPolled[key] = false;
        keysLifted[key] = false;
        keysDown[key] = false;
    } else {
        keysPolled[key] = true;
    }
    numberQueue.Post(ret);
}

/* A key has been pressed.  We may receive multiple key down events due
   to auto repeating. */
void ContinuingLogoApp::OnKeyDown(wxKeyEvent& event) {
    int key = event.GetKeyCode();
    if(!keysDown[key]) {
        keysDown[key] = true;
        keysPolled[key] = false;
        keysLifted[key] = false;
    }
    if(inputmode != POLLMODE)
        event.Skip();
}

/* A key has been lifted.  We only forget about it being down if it
   has been polled.  Otherwise, we mark it as lifted and wait for it
   to be polled before clearing it. */
void ContinuingLogoApp::OnKeyUp(wxKeyEvent& event) {
    int key = event.GetKeyCode();
    if(keysPolled[key]) {
        keysPolled[key] = false;
        keysLifted[key] = false;
        keysDown[key] = false;
    } else {
        keysLifted[key] = true;
    }
    if(inputmode != POLLMODE)
        event.Skip();
}

/* Copy the background (drawings) to the foreground, and then draw the
   turtles on the foreground. */
static void update_foreground() {
    *Foreground = *Background;

    for(IntTurtleMap::iterator i = TurtleMap.begin();
        i != TurtleMap.end();
        i++) {
            i->second.drawtext(*Foreground);
            if(i->second.shown) {
                i->second.draw(*Foreground);
            }
    }
}

void ContinuingLogoApp::paint_canvas(wxDC &dc) {
    dc.DrawBitmap(*Foreground, 0, 0, false);
}

/* Paint the graphics canvas containing the turtles. */
void ContinuingLogoApp::OnPaintCanvas(wxPaintEvent &event) {
    wxPaintDC dc(canvas);
    if(autorefresh)
        update_foreground();
    paint_canvas(dc);
}

/* Let's the interpreter thread tell us to repaint. */
void ContinuingLogoApp::OnPaint(wxPaintEvent &event) {
    canvas->Refresh(false);
}

void ContinuingLogoApp::OnDrawArc(DrawEvent &event) {
    TurtleMap[event.turtle].arc(event.angle, event.radius);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnCleanScreen(DrawEvent &event) {
    wxMemoryDC dc;

    dc.SelectObject(*Background);
    dc.SetBackground(wxBrush(BackgroundColour));
    dc.Clear();
    dc.SelectObject(wxNullBitmap);

    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnClearScreen(DrawEvent &event) {
    for(IntTurtleMap::iterator i = TurtleMap.begin();
        i != TurtleMap.end();
        i++) {
            i->second.x = i->second.y =
            i->second.heading = i->second.rotation = 0;
    }
    OnCleanScreen(event);
}

void ContinuingLogoApp::OnXcor(DrawEvent &event) {
    numberQueue.Post(TurtleMap[event.turtle].x);
}

void ContinuingLogoApp::OnYcor(DrawEvent &event) {
    numberQueue.Post(TurtleMap[event.turtle].y);
}

void ContinuingLogoApp::OnRotation(DrawEvent &event) {
    numberQueue.Post(TurtleMap[event.turtle].rotation);
}

void ContinuingLogoApp::OnSetRotation(DrawEvent &event) {
    TurtleMap[event.turtle].setrotation(event.angle);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnRotate(DrawEvent &event) {
    TurtleMap[event.turtle].rotate(event.angle);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnHeading(DrawEvent &event) {
    numberQueue.Post(TurtleMap[event.turtle].heading);
}

void ContinuingLogoApp::OnSetHeading(DrawEvent &event) {
    TurtleMap[event.turtle].setheading(event.angle);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnForward(DrawEvent &event) {
    TurtleMap[event.turtle].forward(event.distance);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnRight(DrawEvent &event) {
    TurtleMap[event.turtle].right(event.angle);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnSetxy(DrawEvent &event) {
    TurtleMap[event.turtle].moveto(event.x1, event.y1);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnSetx(DrawEvent &event) {
    TurtleMap[event.turtle].moveto(event.x1, TurtleMap[event.turtle].y);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnSety(DrawEvent &event) {
    TurtleMap[event.turtle].moveto(TurtleMap[event.turtle].x, event.y1);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

/* Post in reverse order because the interpreter will prepend these
   to the beginning of an empty list, reversing them. */
void ContinuingLogoApp::OnFetchTurtles(DrawEvent &event) {
    numberQueue.Post(TurtleMap.size());
    for(IntTurtleMap::reverse_iterator i = TurtleMap.rbegin();
        i != TurtleMap.rend();
        i++) {
            numberQueue.Post(i->first);
    }
}

void ContinuingLogoApp::OnDeleteTurtle(DrawEvent &event) {
    TurtleMap.erase(event.turtle);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnPenup(DrawEvent& event) {
    TurtleMap[event.turtle].pendown = false;
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnPendown(DrawEvent& event) {
    TurtleMap[event.turtle].pendown = true;
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnShowturtle(DrawEvent& event) {
    TurtleMap[event.turtle].shown = true;
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnHideturtle(DrawEvent& event) {
    TurtleMap[event.turtle].shown = false;
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnBitmapTurtle(DrawEvent& event) {
    wxBitmap bm;
    if(!bm.LoadFile(event.s)) {
        numberQueue.Post(0);
        return;
    }

    TurtleMap[event.turtle] = Turtle(bm, event.x1, event.y1);
    if(autorefresh)
        canvas->Refresh(false);
    numberQueue.Post(1);
}

void ContinuingLogoApp::OnPathTurtle(DrawEvent& event) {
    TurtleMap[event.turtle] = Turtle();
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnWrap(DrawEvent& event) {
    TurtleMap[event.turtle].mode = TurtleTypes::WRAP;
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnWindow(DrawEvent& event) {
    TurtleMap[event.turtle].mode = TurtleTypes::WINDOW;
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnFence(DrawEvent& event) {
    TurtleMap[event.turtle].mode = TurtleTypes::FENCE;
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnTurtleMode(DrawEvent& event) {
    numberQueue.Post(TurtleMap[event.turtle].mode);
}

void ContinuingLogoApp::OnShown(DrawEvent& event) {
    numberQueue.Post(TurtleMap[event.turtle].shown);
}

void ContinuingLogoApp::OnPendownp(DrawEvent& event) {
    numberQueue.Post(TurtleMap[event.turtle].pendown);
}

void ContinuingLogoApp::OnSetbg(DrawEvent& event) {
    BackgroundColour = wxColour(event.red, event.green, event.blue);
    OnCleanScreen(event);
}

void ContinuingLogoApp::OnSetpencolor(DrawEvent& event) {
    TurtleMap[event.turtle].pen.SetColour(event.red, event.green, event.blue);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnSetpensize(DrawEvent& event) {
    TurtleMap[event.turtle].pen.SetWidth(event.x1);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnPensize(DrawEvent& event) {
    numberQueue.Post(TurtleMap[event.turtle].pen.GetWidth());
}

void ContinuingLogoApp::OnCapturebitmap(DrawEvent& event) {
    update_foreground();

    wxRect rect(250 + event.x1, 250 - event.y1,
                event.x2 - event.x1 + 1, event.y1 - event.y2 + 1);
    wxBitmap newbm = Foreground->GetSubBitmap(rect);
    if(event.transparent) {
        newbm.SetMask(new wxMask(newbm, BackgroundColour));
    }

    TurtleMap[event.turtle].setbitmap(newbm, event.xcenter - event.x1,
                                             event.y1 - event.ycenter);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnCaptureshape(DrawEvent& event) {
    update_foreground();

    wxRect rect(250 + event.x1, 250 - event.y1,
                event.x2 - event.x1 + 1, event.y1 - event.y2 + 1);
    wxBitmap newbm = Foreground->GetSubBitmap(rect);
    if(event.transparent) {
        newbm.SetMask(new wxMask(newbm, BackgroundColour));
    }

    /* "turtle" in this context refers to the shape number. */
    ShapeMap[event.turtle] = Shape(newbm,
                                   event.xcenter - event.x1,
                                   event.y1 - event.ycenter);

    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnSetshape(DrawEvent& event) {
    int shapenum = (int)event.x1; /* Set from second argument to the
                                     constructor in global_environment.c */
    if(ShapeMap.find(shapenum) != ShapeMap.end()) {
        Shape s = ShapeMap[shapenum];
        TurtleMap[event.turtle].setbitmap(s.bm, s.xoffset, s.yoffset);
        if(autorefresh)
            canvas->Refresh(false);
    }

    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnAutorefreshoff(DrawEvent& event) {
    autorefresh = false;
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnAutorefreshon(DrawEvent& event) {
    autorefresh = true;
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

/* Must use a wxClientDC when outside of a paint event handler. */
void ContinuingLogoApp::OnRefresh(DrawEvent& event) {
    wxClientDC dc(canvas);
    update_foreground();
    paint_canvas(dc);

    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnSetxscale(DrawEvent& event) {
    TurtleMap[event.turtle].xscale = event.x1;
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnSetyscale(DrawEvent& event) {
    TurtleMap[event.turtle].yscale = event.x1;
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnXscale(DrawEvent& event) {
    numberQueue.Post(TurtleMap[event.turtle].xscale);
}

void ContinuingLogoApp::OnYscale(DrawEvent& event) {
    numberQueue.Post(TurtleMap[event.turtle].yscale);
}

void ContinuingLogoApp::OnSaveTurtle(DrawEvent& event) {
    TurtleMap[event.turtle].save(event.s);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnSavePict(wxCommandEvent& event) {
    Background->SaveFile(event.GetString(), wxBITMAP_TYPE_PNG);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnLoadPict(wxCommandEvent& event) {
    Background->LoadFile(event.GetString(), wxBITMAP_TYPE_PNG);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnPenPaint(DrawEvent& event) {
    TurtleMap[event.turtle].drawmode = TurtleTypes::PAINT;
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnPenErase(DrawEvent& event) {
    TurtleMap[event.turtle].drawmode = TurtleTypes::ERASE;
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

/* Post in reverse order because the interpreter will prepend these
   to the beginning of an empty list, reversing them. */
void ContinuingLogoApp::OnBackground(DrawEvent& event) {
    numberQueue.Post(BackgroundColour.Blue());
    numberQueue.Post(BackgroundColour.Green());
    numberQueue.Post(BackgroundColour.Red());
}

/* Post in reverse order because the interpreter will prepend these
   to the beginning of an empty list, reversing them. */
void ContinuingLogoApp::OnPencolor(DrawEvent& event) {
    wxColour c = TurtleMap[event.turtle].pen.GetColour();
    numberQueue.Post(c.Blue());
    numberQueue.Post(c.Green());
    numberQueue.Post(c.Red());
}

void ContinuingLogoApp::OnPenmode(DrawEvent& event) {
    numberQueue.Post(TurtleMap[event.turtle].drawmode);
}

void ContinuingLogoApp::OnFill(DrawEvent& event) {
    TurtleMap[event.turtle].fill();
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnLabel(DrawEvent& event) {
    TurtleMap[event.turtle].label(event.s);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnSettext(DrawEvent& event) {
    TurtleMap[event.turtle].text = event.s;
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

void ContinuingLogoApp::OnGettext(DrawEvent& event) {
    editedQueue.Post(TurtleMap[event.turtle].text);
}

/* During ordinary display, turtles draw themselves on the foreground,
   which gets rebuilt during every redisplay.
   To stamp, we have the turtle draw itself on the background, so the
   image will stay there. */
void ContinuingLogoApp::OnStamp(DrawEvent& event) {
    TurtleMap[event.turtle].draw(*Background);
    if(autorefresh)
        canvas->Refresh(false);
    turtleConditionLocker.Lock();
    turtleCondition.Signal();
    turtleConditionLocker.Unlock();
}

/* Return the color of the pixel under the turtle. */
void ContinuingLogoApp::OnColorunder(DrawEvent& event) {
	Turtle &t = TurtleMap[event.turtle];

    /* Figure out which background pixel the turtle is over. */
    int bgx = 250 + round(t.x);
    int bgy = 250 - round(t.y);
    int maxx = Background->GetWidth();
    int maxy = Background->GetHeight();

    /* Return black if it's off the screen. */
    if(bgx < 0 || bgx >= maxx || bgy < 0 || bgy >= maxy) {
		numberQueue.Post(0);
		numberQueue.Post(0);
		numberQueue.Post(0);
        return;
	}

    /* Convert the background to a wxImage so we can examine its
       pixels. */
    wxImage im = Background->ConvertToImage();

    /* Fetch the color components of the pixel under the turtle
       and send them back to the interpreter. */
    numberQueue.Post(im.GetBlue(bgx, bgy));
    numberQueue.Post(im.GetGreen(bgx, bgy));
    numberQueue.Post(im.GetRed(bgx, bgy));
}

/* Return the color of the pixel under the turtle relative (x, y)
   coordinate. */
void ContinuingLogoApp::OnXyColorunder(DrawEvent& event) {
	Turtle &t = TurtleMap[event.turtle];
    wxGraphicsMatrix gm = t.matrix();

	/* Convert to screen coordinates */
    double tx = event.x1, ty = -event.y1;
    gm.TransformPoint(&tx, &ty);
    if(t.mode == TurtleTypes::WRAP) {
        tx = fmod(tx, 501); if(tx < 0) tx += 501;
        ty = fmod(ty, 501); if(ty < 0) ty += 501;
    }

    int bgx = round(tx), bgy = round(ty);

    int maxx = Background->GetWidth();
    int maxy = Background->GetHeight();

    /* Return black if it's off the screen. */
    if(bgx < 0 || bgx >= maxx || bgy < 0 || bgy >= maxy) {
		numberQueue.Post(0);
		numberQueue.Post(0);
		numberQueue.Post(0);
        return;
	}

    /* Convert the background to a wxImage so we can examine its
       pixels. */
    wxImage im = Background->ConvertToImage();

    /* Fetch the color components of the pixel under the turtle
       and send them back to the interpreter. */
    numberQueue.Post(im.GetBlue(bgx, bgy));
    numberQueue.Post(im.GetGreen(bgx, bgy));
    numberQueue.Post(im.GetRed(bgx, bgy));
}

void ContinuingLogoApp::OnOver(CollisionTestEvent& event) {
    numberQueue.Post(TurtleMap[event.turtle].over(event.criteria1,
                                                  event.criteria2));
}

void ContinuingLogoApp::OnXyOver(CollisionTestEvent& event) {
    numberQueue.Post(TurtleMap[event.turtle].over(event.x,
                                                  event.y,
                                                  event.criteria1));
}

void ContinuingLogoApp::OnTouching(CollisionTestEvent& event) {
    numberQueue.Post(TurtleMap[event.turtle].touching(event.criteria1,
                                                      event.criteria2,
                                                      TurtleMap[event.turtle2]));
}

void ContinuingLogoApp::OnXyTouching(CollisionTestEvent& event) {
    numberQueue.Post(TurtleMap[event.turtle].touching(event.x,
                                                      event.y,
                                                      TurtleMap[event.turtle2]));
}

void ContinuingLogoApp::OnWhosTouching(CollisionTestEvent& event) {
    IntVector found;

    /* Force the turtle to exist */
    TurtleMap[event.turtle];

    /* Reverse order because interpreter prepends responses onto
       beginning of a linked list, reversing them. */
    for(IntTurtleMap::reverse_iterator i = TurtleMap.rbegin();
        i != TurtleMap.rend();
        i++)
        if(i->first != event.turtle &&
           TurtleMap[event.turtle].touching(event.criteria1,
                                            event.criteria2,
                                            i->second))
            found.push_back(i->first);

    numberQueue.Post(found.size());

    for(IntVector::iterator i = found.begin(); i != found.end(); i++)
        numberQueue.Post(*i);
}

void ContinuingLogoApp::OnXyWhosTouching(CollisionTestEvent& event) {
    IntVector found;

    /* Force the turtle to exist */
    TurtleMap[event.turtle];

    /* Reverse order because interpreter prepends responses onto
       beginning of a linked list, reversing them. */
    for(IntTurtleMap::reverse_iterator i = TurtleMap.rbegin();
        i != TurtleMap.rend();
        i++)
        if(i->first != event.turtle &&
           TurtleMap[event.turtle].touching(event.x,
                                            event.y,
                                            i->second))
            found.push_back(i->first);

    numberQueue.Post(found.size());

    for(IntVector::iterator i = found.begin(); i != found.end(); i++)
        numberQueue.Post(*i);
}

void ContinuingLogoApp::OnGlobalXyWhosTouching(CollisionTestEvent& event) {
    IntVector found;

    /* Reverse order because interpreter prepends responses onto
       beginning of a linked list, reversing them. */
    for(IntTurtleMap::reverse_iterator i = TurtleMap.rbegin();
        i != TurtleMap.rend();
        i++) {
        CollisionTester tester(i->second, PixelCriteria(NONTRANSPARENT));

        double tx = event.x + 250, ty = 250 - event.y;
        if(tester.CollidesWith(tx, ty))
            found.push_back(i->first);
    }

    numberQueue.Post(found.size());

    for(IntVector::iterator i = found.begin(); i != found.end(); i++)
        numberQueue.Post(*i);
}

/* Used by logoreader.c to get a pointer to the queue so the interpreter
   thread can receive the input strings that we send to it. */
StringQueue *ContinuingLogoApp::GetStringQueue() {
    return &queue;
}

/* Used by WXEDIT/wxedit_subr to get a pointer to the editedQueue so that
   it can receive the edited text that we send back to it. */
StringQueue *ContinuingLogoApp::GetEditedQueue() {
    return &editedQueue;
}

/* Used to send numbers back to the interpreter. */
NumberQueue *ContinuingLogoApp::GetNumberQueue() {
    return &numberQueue;
}

/* Entry point for the interpreter thread. Calls wxstart_logo() from
   main.c to get the interpreter going. */
wxThread::ExitCode InterpreterThread::Entry() {
    wxstart_logo();
    return (wxThread::ExitCode)0;
}


