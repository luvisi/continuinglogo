
#ifndef TURTLES_H
#define TURTLES_H

#include <map>
#include <execinfo.h>
#include "wxui.h"
#include "trig.h"

/* Convert degrees to radians. */
static inline double DegToRad(double deg) { return (deg * M_PI) / 180.0; }

/* CriteriaType and PixelCriteria are used to specify which pixels in
   a bitmap will be considered during a collision test. */
enum CriteriaType {
    ALL,             /* All pixels. */
    NONTRANSPARENT,  /* Non-transparent pixels. */
    COLOUR,          /* Pixels of a particular color. */
    PAST_TOP,        /* Positions past the top of the screen. */
    PAST_BOTTOM,     /* Positions past the bottom of the screen. */
    PAST_LEFT,       /* Positions past the left of the screen. */
    PAST_RIGHT       /* Positions past the right of the screen. */
};

/* A PixelCriteria is a predicate that informs collision detection
   of whether a pixel at a particular location in a particular
   bitmap is of interest or not. */
class PixelCriteria {
public:
    PixelCriteria() {}
    PixelCriteria(CriteriaType type) : type(type) {}
    PixelCriteria(CriteriaType type, wxColour colour) :
        type(type), colour(colour) {}
    bool matches(wxImage &im, int x, int y) {
        int width = im.GetWidth();
        int height = im.GetHeight();

        /* If the pixel is outside of the bitmap, and we are not
           testing one of the PAST_* types, then it is not of interest. */
        if((x < 0 || x >= width || y < 0 || y >= height) &&
           (type != PAST_TOP && type != PAST_BOTTOM &&
            type != PAST_LEFT && type != PAST_RIGHT))
            return false;

        switch(type) {
            case ALL:
                /* If every pixel is of interest, we always return true. */
                return true;
            case NONTRANSPARENT:
                /* If there is no alpha/transparency channel, then no
                   pixels are transparent. */
                if(!im.HasAlpha())
                    return true;
                return im.GetAlpha(x, y) != wxIMAGE_ALPHA_TRANSPARENT;
            case COLOUR:
                /* If we're testing for a particular color, then only
                   pixels of that color return true. */
                {
                    wxColour c(im.GetRed(x, y),
                               im.GetGreen(x, y),
                               im.GetBlue(x, y));
                    return colour == c;
                }

            /* Tests for coordinates past the edges. */
            case PAST_TOP: return y < 0;
            case PAST_BOTTOM: return y >= height;
            case PAST_LEFT: return x < 0;
            case PAST_RIGHT: return x >= width;
        }
        return false;
    }

    CriteriaType type;
    wxColour colour;
};

/* CollisionTestEvent's are sent from the interpreter thread to the GUI
   thread to ask whether a particular collision has occured.
   There are several types of events, and they use different subsets of
   the member variables. */
class CollisionTestEvent : public wxEvent {
public:
    /* Used for TURTLE_OVER.  criteria1 specifies pixels in the
       turtle.  criteria2 specifies pixels in the background. */
    /* Used for TURTLE_WHOSTOUCHING.  Which turtles have pixels matching
       criteria2 that are touching a pixel in turtle matching criteria1? */
    CollisionTestEvent(wxEventType eventType,
                       int turtle,
                       PixelCriteria criteria1,
                       PixelCriteria criteria2) :
        wxEvent(0, eventType),
        turtle(turtle),
        criteria1(criteria1),
        criteria2(criteria2) { }

    /* Used for TURTLE_PASTTOP, TURTLE_PASTBOTTOM, TURTLE_PASTLEFT,
       TURTLE_PASTRIGHT. */
    CollisionTestEvent(wxEventType eventType,
                       int turtle,
                       PixelCriteria criteria1) :
        wxEvent(0, eventType),
        turtle(turtle),
        criteria1(criteria1) { }

    /* Used in TURTLE_TOUCHING.  Are any pixels matching criteria1 in
       turtle touching any pixels matching criteria2 in turtle2? */
    CollisionTestEvent(wxEventType eventType,
                       int turtle,
                       int turtle2,
                       PixelCriteria criteria1,
                       PixelCriteria criteria2) :
        wxEvent(0, eventType),
        turtle(turtle),
        turtle2(turtle2),
        criteria1(criteria1),
        criteria2(criteria2) { }
    /* Used for GLOBALXY_WHOSTOUCHING. Which turtles have a pixel matching
       criteria1 that's over point (x, y) of the background? */
    CollisionTestEvent(wxEventType eventType,
                       int x,
                       int y,
                       PixelCriteria criteria1) :
        wxEvent(0, eventType),
        x(x),
        y(y),
        criteria1(criteria1) { }
    /* Used for TURTLEXY_OVER.  Tests whether a particular pixel in the
       turtle is over a background pixel that meets criteria1. */
    CollisionTestEvent(wxEventType eventType,
                       int turtle,
                       int x,
                       int y,
                       PixelCriteria criteria1) :
        wxEvent(0, eventType),
        turtle(turtle),
        x(x),
        y(y),
        criteria1(criteria1) { }
    /* Used for TURTLEXY_TOUCHING.  Is a particular pixel in turtle
       touching any non-transparent pixels in turtle2? */
    CollisionTestEvent(wxEventType eventType,
                       int turtle,
                       int x,
                       int y,
                       int turtle2) :
        wxEvent(0, eventType),
        turtle(turtle),
        turtle2(turtle2),
        x(x),
        y(y) { }
    /* Used for TURTLEXY_WHOSTOUCHING.  Which turtles have a non-transparent
       pixel touching pixel (x, y) of turtle? */
    CollisionTestEvent(wxEventType eventType,
                       int turtle,
                       int x,
                       int y) :
        wxEvent(0, eventType),
        turtle(turtle),
        x(x),
        y(y) { }
    virtual wxEvent *Clone() const { return new CollisionTestEvent(*this); }
    int turtle, turtle2;
    int x, y;
    PixelCriteria criteria1, criteria2;
};

wxDECLARE_EVENT(TURTLE_OVER, CollisionTestEvent);
wxDECLARE_EVENT(TURTLE_PASTTOP, CollisionTestEvent);
wxDECLARE_EVENT(TURTLE_PASTBOTTOM, CollisionTestEvent);
wxDECLARE_EVENT(TURTLE_PASTLEFT, CollisionTestEvent);
wxDECLARE_EVENT(TURTLE_PASTRIGHT, CollisionTestEvent);
wxDECLARE_EVENT(TURTLEXY_OVER, CollisionTestEvent);
wxDECLARE_EVENT(TURTLE_TOUCHING, CollisionTestEvent);
wxDECLARE_EVENT(TURTLEXY_TOUCHING, CollisionTestEvent);
wxDECLARE_EVENT(TURTLE_WHOSTOUCHING, CollisionTestEvent);
wxDECLARE_EVENT(TURTLEXY_WHOSTOUCHING, CollisionTestEvent);
wxDECLARE_EVENT(GLOBALXY_WHOSTOUCHING, CollisionTestEvent);

/* Enumerations that will be used for some turtle member variables. */
namespace TurtleTypes {
    /* What happens when the turtle gets to the edge of the window? */
    enum TurtleMode {
        WRAP,   /* Wrap around to the opposite edge. */
        WINDOW, /* Continue off the screen, into unseen portions of an
                   infinite canvas. */
        FENCE   /* Fail to pass the edge of the screen. */
    };

    /* What does the turtle do to the pixels it walks over when the
       pen is down? */
    enum DrawMode {
        PAINT,  /* Draw on the background with the turtle's color. */
        ERASE,  /* Erase the background/draw the background color. */
        REVERSE /* Reverse the pixels (UNIMPLEMENTED). */
    };
};

/* Virtual class that represents the internals of a turtle whose
   implementations will vary between different types. */
class TurtleGuts {
public:
    virtual ~TurtleGuts() { }
    virtual TurtleGuts *clone() = 0;
    /* Draw the turtle onto a bitmap.  The target is the foreground for
       normal display, and the background for implementing STAMP. */
    virtual void draw(wxBitmap &target, double x, double y,
                      double heading, double xscale, double yscale,
                      wxPen pen) = 0;
    virtual wxRect2DDouble boundingbox(wxGraphicsMatrix transform) = 0;
    
    /* Save the turtle's image into a file. */
    virtual void save(wxString &file) = 0;
    /* Are any pixels in this turtle that match criteria1 over any pixels
       of the background tha tmatch criteria2? */
    virtual bool over(PixelCriteria criteria1,
                      PixelCriteria criteria2,
                      wxGraphicsMatrix gm,
                      TurtleTypes::TurtleMode mode) = 0;
    /* Are any pixels in this turtle that match criteria1 touching any pixels
       in other that match criteria2? */
    virtual bool touching(PixelCriteria criteria1,
                          PixelCriteria criteria2,
                          wxGraphicsMatrix gm,
                          class Turtle &other,
                          TurtleTypes::TurtleMode mode) = 0;
    /* Does pixel (x, y) of this turtle match criteria criteria? */
    virtual bool matches(PixelCriteria criteria, int x, int y) = 0;
    /* Generate the translation matrix that maps pixels in the turtle
       to pixels on the background. */
    virtual wxGraphicsMatrix matrix(double x, double y, double heading, double xscale, double yscale) = 0;
};

/* BitmapTurtle's contains a bitmap that is printed to the foreground
   when the turtle is displayed and printed to the background when the
   turtle is STAMP'd. */
class BitmapTurtle : public TurtleGuts {
public:
    BitmapTurtle(wxBitmap bm, int xoffset, int yoffset) 
    : bm(bm), xoffset(xoffset), yoffset(yoffset) { }
    ~BitmapTurtle() { }
    virtual TurtleGuts *clone() { return new BitmapTurtle(bm, xoffset, yoffset); }
    virtual void draw(wxBitmap &target, double x, double y, double heading, double xscale, double yscale, wxPen pen);
    virtual wxRect2DDouble boundingbox(wxGraphicsMatrix transform);
    virtual void save(wxString &file);
    virtual bool over(PixelCriteria criteria1,
                      PixelCriteria criteria2,
                      wxGraphicsMatrix gm,
                      TurtleTypes::TurtleMode mode);
    virtual bool touching(PixelCriteria criteria1,
                          PixelCriteria criteria2,
                          wxGraphicsMatrix gm,
                          class Turtle &other,
                          TurtleTypes::TurtleMode mode);
    virtual bool matches(PixelCriteria criteria, int x, int y);
    virtual wxGraphicsMatrix matrix(double x, double y, double heading, double xscale, double yscale) {
        wxGraphicsMatrix gm = wxGraphicsRenderer::GetDefaultRenderer()->CreateMatrix();
        /* Move top left corner of bitmap to center of display location. */
        gm.Translate(250 + x, 250 - y);
        /* Rotate the bitmap. */
        gm.Rotate(DegToRad(heading));
        /* Move the top left corner of the bitmap up and to the left
           (virtually speaking) by the offsets, scaled appropriately. */
        gm.Translate(-xoffset*xscale, -yoffset*yscale);
        /* Shrink/grow the turtle according to scale. */
        gm.Scale(xscale, yscale);
        return gm;
    }
private:
    /* Bitmap to display */
    wxBitmap bm;

    /* Display coordinates (with (0, 0) at the top left, positive y axis down)
       of the "center" pixel of the bitmap, the one that will be over
       the turtle's "location" on the screen and about which the turtle
       will rotate. */
    int xoffset, yoffset;
};

/* A PathTurtle represents a plain triangular turtle, drawn on the screen
   using a wxGraphicsPath. */
class PathTurtle : public TurtleGuts {
public:
    PathTurtle(wxGraphicsPath &path) : path(path) { }
    ~PathTurtle() { }
    virtual TurtleGuts *clone() { return new PathTurtle(path); }
    virtual void draw(wxBitmap &target, double x, double y, double heading, double xscale, double yscale, wxPen pen);
    virtual wxRect2DDouble boundingbox(wxGraphicsMatrix transform);
    virtual void save(wxString &file) {}
    virtual bool over(PixelCriteria criteria1,
                      PixelCriteria criteria2,
                      wxGraphicsMatrix gm,
                      TurtleTypes::TurtleMode mode);
    virtual bool touching(PixelCriteria criteria1,
                          PixelCriteria criteria2,
                          wxGraphicsMatrix gm,
                          class Turtle &other,
                          TurtleTypes::TurtleMode mode);
    /* Match any location inside the triangle.  We're working in screen
       coordinates here, not logo coordinates. */
    virtual bool matches(PixelCriteria criteria, int x, int y) {
        return y <= 0 && 6*y + 19*x >= -114 && 6*y - 19*x >= -114;
    }

    /* This is a more direct transformation than for bitmaps since the path
       is created already centered on (0, 0). */
    virtual wxGraphicsMatrix matrix(double x, double y, double heading, double xscale, double yscale) {
        wxGraphicsMatrix gm = wxGraphicsRenderer::GetDefaultRenderer()->CreateMatrix();
        gm.Translate(250 + x, 250 - y);
        gm.Rotate(DegToRad(heading));
        gm.Scale(xscale, yscale);
        return gm;
    }
private:
    wxGraphicsPath path;
};

/* Representation of a Turtle.
   This handles the generic turtle actions that are the same regardless
   of type. */
class Turtle {
public:

    Turtle(wxBitmap bm, int xoffset, int yoffset) :
        x(0), y(0), heading(0), rotation(0), xscale(1), yscale(1),
        shown(true), pendown(true), mode(TurtleTypes::WRAP),
        drawmode(TurtleTypes::PAINT), pen(*wxWHITE_PEN),
        tg(new BitmapTurtle(bm, xoffset, yoffset)) {
        pen.SetCap(wxCAP_PROJECTING);
    }

    Turtle(wxGraphicsPath &path) :
        x(0), y(0), heading(0), rotation(0), xscale(1), yscale(1),
        shown(true), pendown(true), mode(TurtleTypes::WRAP),
        drawmode(TurtleTypes::PAINT), pen(*wxWHITE_PEN),
        tg(new PathTurtle(path)) {
        pen.SetCap(wxCAP_PROJECTING);
    }

    /* The default turtle contains a PathTurtle that draws a triangle. */
    Turtle() :
        x(0), y(0), heading(0), rotation(0), xscale(1), yscale(1),
        shown(true), pendown(true), mode(TurtleTypes::WRAP),
        drawmode(TurtleTypes::PAINT), pen(*wxWHITE_PEN) { 
        pen.SetCap(wxCAP_PROJECTING);
        wxGraphicsRenderer *gr = wxGraphicsRenderer::GetDefaultRenderer();
        wxGraphicsPath path = gr->CreatePath() ;
        /* Create the path to draw the default triangle.
           These are in screen coordinates.
           (0, 0) is the top left pixel and the positive y
           axis is down. */
        path.MoveToPoint(0, 0);
        path.AddLineToPoint(-6, 0);
        path.AddLineToPoint(0, -19);
        path.AddLineToPoint(6, 0);
        path.AddLineToPoint(0, 0);
        path.CloseSubpath();
        tg = new PathTurtle(path);
    }

    Turtle(const Turtle &t) :
        x(t.x), y(t.y), heading(t.heading), rotation(t.rotation),
        xscale(t.xscale), yscale(t.yscale), shown(t.shown),
        pendown(t.pendown), mode(t.mode), drawmode(t.drawmode),
        pen(t.pen), tg(t.tg ? t.tg->clone() : 0) { }

    Turtle &operator=(const Turtle &t) {
        if(&t == this) return *this;
        x = t.x; y = t.y;
        heading = t.heading; rotation = t.rotation;
        xscale = t.xscale; yscale = t.yscale;
        shown = t.shown; pendown = t.pendown;
        mode = t.mode;
        drawmode = t.drawmode;
        pen = t.pen;
        if(tg) delete(tg);
        tg = t.tg->clone();
        return *this;
    }

    ~Turtle() { if(tg) delete tg; }

    /* Draw the turtle by telling the TurtleGuts member to draw
       itself with a particular location, heading, rotation, xscale,
       yscale, and pen.  These are all tracked the same for all turtles. */
    void draw(wxBitmap &target) {
        if(tg) {
            double xoff, yoff;
            if(mode == TurtleTypes::WRAP) {
                /* Handle wrap mode by drawing the turtle 9 times. */
                for(xoff = -501; xoff <= 501; xoff += 501)
                    for(yoff = -501; yoff <= 501; yoff += 501)
                        if(xoff != 0 || yoff != 0)
                            tg->draw(target,
                                     x+xoff,
                                     y+yoff,
                                     heading+rotation,
                                     xscale,
                                     yscale,
                                     pen);
            }
            tg->draw(target, x, y, heading+rotation, xscale, yscale, pen);
        }
    }

    wxRect2DDouble boundingbox() {
        return tg->boundingbox(matrix());
    }

    void drawtext(wxBitmap &target) {
		if(text.length() > 0) {
			int bgx = 250 + round(x);
			int bgy = 250 - round(y);

			wxMemoryDC dc;
			dc.SelectObject(target);

			/* Set the background and forground for the text to print. */
			dc.SetBackgroundMode(wxTRANSPARENT);
			dc.SetTextForeground(pen.GetColour());

			/* Use the system default font. */
			dc.SetFont(*wxNORMAL_FONT);

			/* wxWidgets prints text below and to the right
			   of the given location.
			   ucblogo prints text above and to the right
			   of the given location.  To print the text
			   above the turtle's location, we calculate the
			   height of the text and raise the y coordinate
			   by that far. */

			wxSize size = dc.GetMultiLineTextExtent(text);
            double rotated = heading+rotation;
            int height = size.GetHeight();
			bgy -= height*degreecos(rotated);
            bgx += height*degreesin(rotated);
			dc.DrawRotatedText(text, bgx, bgy, -rotated);
			dc.SelectObject(wxNullBitmap);
        }
    }

    /* Pass several of these through to our TurtleGuts. */

    bool over(PixelCriteria criteria1, PixelCriteria criteria2) {
        return tg->over(criteria1, criteria2, matrix(), mode);
    }

    bool over(int relativex, int relativey, PixelCriteria criteria);

    bool touching(PixelCriteria criteria1,
                  PixelCriteria criteria2,
                  Turtle &other) {
        return tg->touching(criteria1, criteria2, matrix(), other, mode);
    }

    bool touching(int relativex, int relativey, Turtle &other);

    bool matches(PixelCriteria criteria, int x, int y) {
        return tg->matches(criteria, x, y);
    }

    wxGraphicsMatrix matrix(double xoffset = 0, double yoffset = 0) {
        return tg->matrix(x+xoffset, y+yoffset, heading+rotation,
                          xscale, yscale);
    }

    /* Actions that are the same for all turtles. */
    void forward(double dist);
    void moveto(double x, double y);
    void arc(double angle, double radius);
    void right(double degrees);
    void setheading(double newheading);
    void rotate(double degrees);
    void setrotation(double newrotation);
    void setbitmap(wxBitmap bitmap, int xoffset, int yoffset);
    void save(wxString &file) { tg->save(file); }
    void fill();
    void label(wxString &s);

    double x, y; /* Location of the turtle in Logo coordinates.
                    (0, 0) is in the center of the screen, and the
                    positive y axis is up. */
    double heading; /* Direction the turtle is pointing, and direction
                       in which it moves FORWARD. */
    double rotation; /* Additional rotation for display only purposes.
                        Added to heading. */
    double xscale, yscale; /* Squish/expand the turtle in the x and y
                              directions.  Scaling is according to
                              turtle coordinates. */
    bool shown;   /* Is the turtle visible? */
    bool pendown; /* Is the pen down/Is the turtle drawing? */

    TurtleTypes::TurtleMode mode;   /* WRAP, WINDOW, or FENCE */
    TurtleTypes::DrawMode drawmode; /* PAINT, ERASE, or REVERSE */

    wxPen pen; /* Pen used to draw with. */

    wxString text; /* Displayed next to the turtle, even when hidden. */

private:
    TurtleGuts *tg;
};

/* A CollisionTester has one useful member function, CollidesWith().
   It tells whether a particular turtle, using a particular criteria
   (both provided at creation time) collides with a given set of
   screen coordinate.
   This allows us to create the multiple inverse matrices necessary
   to test for wrapping once, before the loops through pixels
   in the touching() functions. */
class CollisionTester {
public:
    CollisionTester(Turtle &t, PixelCriteria criteria) :
      turtle(t), criteria(criteria) {
        if(t.mode == TurtleTypes::WRAP) {
            for(double xoff = -501; xoff <= 501; xoff += 501)
                for(double yoff = -501; yoff <= 501; yoff += 501) {
                    matrices.push_back(t.matrix(xoff, yoff));
                    matrices.back().Invert();
                }
        } else {
            matrices.push_back(t.matrix());
            matrices.back().Invert();
        }
    }

    /* Does the turtle have a pixel matching criteria that is over
       the screen position (screenx, screeny)?
       screenx and screeny are in screen coordinates, with (0, 0) at
       the upper left corner and the positive y axis downward. */
    bool CollidesWith(double screenx, double screeny) {
        for(vector<wxGraphicsMatrix>::iterator it = matrices.begin();
            it != matrices.end();
            it++) {
                double tx = screenx, ty = screeny;
                it->TransformPoint(&tx, &ty);
                int otherx = round(tx), othery = round(ty);
                if(turtle.matches(criteria, otherx, othery))
                    return true;
        }
        return false;
    }

private:
    Turtle &turtle;
    PixelCriteria criteria;
    vector<wxGraphicsMatrix> matrices;
};

#endif /* TURTLES_H */
