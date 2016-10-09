
#include "wxui.h"
#include "turtles.h"
#include "global_environment.h"
#include <vector>

/* Draw the bitmap for this turtle, appropriately scaled, rotated, and
   located.
   This only needs to draw it once.  Turtle::Draw() handles wrapping by
   callig this multiple times. */
void BitmapTurtle::draw(wxBitmap &target, double x, double y, double heading, double xscale, double yscale, wxPen pen) {
    wxMemoryDC dc;
    dc.SelectObject(target);
    dc.SetAxisOrientation(true, true);

    /* We use a wxGraphicsContext because it supports arbitrary
       affine transformations. */
    wxGCDC gcdc(dc);
    wxGraphicsContext *gc = gcdc.GetGraphicsContext();
    gc->SetPen(pen);

    gc->PushState();
    gc->Translate(250 + x, 250 - y);  /* Move (0, 0) of image to (x, y) with
                                         (x, y) in turtle coordinates and the
                                         arguments to Translate() in screen
                                         coordinates. */

    /* Rotate around (x, y). */
    gc->Rotate(DegToRad(heading));

    /* Move top left corner of bitmap by its offset, with appropriate
       scaling. */
    gc->Translate(-xoffset*xscale, -yoffset*yscale);

    /* Scale the bitmap itself. */
    gc->Scale(xscale, yscale);

    gcdc.DrawBitmap(bm, 0, 0, true);
    gc->PopState();
    dc.SelectObject(wxNullBitmap);
}

/* Draw a path turtle.  Slightly simpler than a BitmapTurtle because there
   are no x and y offsets.  The turtle's (0, 0) gets translated directly to
   the target (x, y). */
void PathTurtle::draw(wxBitmap &target, double x, double y, double heading, double xscale, double yscale, wxPen pen) {
    wxMemoryDC dc;
    dc.SelectObject(target);

    wxPen pathpen = pen;
    pathpen.SetWidth(1);

    dc.SetAxisOrientation(true, true);
    wxGCDC gcdc(dc);
    wxGraphicsContext *gc = gcdc.GetGraphicsContext();
    gc->SetPen(pathpen);

    gc->PushState();
    gc->Translate(250 + x, 250 - y);
    gc->Rotate(DegToRad(heading));
    gc->Scale(xscale, yscale);
    gc->StrokePath(path);
    gc->PopState();
    dc.SelectObject(wxNullBitmap);
}

/* FORWARD commands end up here.
   Works by calculating the unwrapped destination and calling moveto().
   moveto() performs any necessary wrapping and line drawing.
   x is sin() and y is cos() because logo angles are measured clockwise
   from the positive y axis.
   x, y, newx, and newy are in logo coordinates with (0, 0) in the
   center of the screen and the positive y axis being up. */
void Turtle::forward(double dist) {
    double newx, newy;
    newx = x + dist * degreesin(heading);
    newy = y + dist * degreecos(heading);
    moveto(newx, newy);
}

void Turtle::setheading(double newheading) {
    heading = fmod(newheading, 360);
    if(heading < 0)
        heading += 360;
}

void Turtle::right(double degrees) {
    setheading(heading + degrees);
}

void Turtle::setrotation(double newrotation) {
    rotation = fmod(newrotation, 360);
    if(rotation < 0)
        rotation += 360;
}

void Turtle::rotate(double degrees) {
    setrotation(rotation + degrees);
}

/* draw_line() draws a single line on the background, without any wrapping.
   It can be called by moveto() multiple times with moveto() handling
   wrapping.
   The pen argument is called by value, so when we modify it we are not
   changing the turtle's "real" color. */
static void draw_line(double x1, double y1, double x2, double y2, wxPen pen, TurtleTypes::DrawMode drawmode) {
    wxMemoryDC dc;

    int rx1 = 250 + round(x1);
    int ry1 = 250 - round(y1);
    int rx2 = 250 + round(x2);
    int ry2 = 250 - round(y2);

    dc.SelectObject(*Background);
    dc.SetLogicalFunction(wxCOPY);
    if(drawmode == TurtleTypes::ERASE) {
        /* If we are in ERASE mode then we are drawing using the background
           color. */
        pen.SetColour(BackgroundColour);
    }
    dc.SetPen(pen);
    dc.DrawLine(rx1, ry1, rx2, ry2);
    dc.SelectObject(wxNullBitmap);
}

/* moveto() takes as arguments the new location to which the turtle
   is moving, which may be off the screen.
   It handles wrapping in WRAP mode and stopping in FENCE mode.
   It is called by forward() and uses draw_line() to draw non-wrapping
   line segments on the screen. */
void Turtle::moveto(double newx, double newy) {
    double ix, iy, /* x or y intercept */
           wx, wy, /* x and y coordinates after wrapping */
           fx, fy; /* onscreen movement as a fraction of total movement
                      for both x and y */
    bool going_left, going_down;

    if(mode == TurtleTypes::WINDOW) {
        /* In WINDOW mode, what is viewed is a window into an "infinite"
           canvas.
           The turtle may start and/or end off the screen.
           We just move the turtle and issue the drawing commands.
           If the lines drawn are off the screen, then they don't show. */
        if(pendown)
            draw_line(x, y, newx, newy, pen, drawmode);
        x = newx;
        y = newy;
        return;
    }

    for(;;) {
        fx = 1; /* Begin by assuming no wrapping */
        fy = 1;

        /* If x wraps then we need to lower fx to the ratio of pre-wrap
           movement to total movement */
        if(newx < -250.5) {
            /* We're moving left past the left edge of the screen.
               Divide the distance to the left edge of the screen by the
               total amount we're moving left. */
            fx = (x - -250.5)/(x - newx);
        } else if(newx > 250.5) {
            /* We're moving right past the right edge of the screen.
               Divide the distance to the right edge of the screen by the
               total amount we're moving right. */
            fx = (250.5 - x)/(newx - x);
        }

        /* If y wraps then we need to lower fy to the ratio of pre-wrap
           movement to total movement */
        if(newy < -250.5) {
            /* We've moving down past the bottom edge of the screen.
               Divide the distance to the bottom edge of the screen by the
               total amount we're moving down. */
            fy = (y - -250.5)/(y - newy);
        } else if(newy > 250.5) {
            /* We've moving up past the top edge of the screen.
               Divide the distance to the top edge of the screen by the
               total amount we're moving up. */
            fy = (250.5 - y)/(newy - y);
        }

        /* Where we intercept with the edge of the screen if we wrap. */
        ix = newx;
        iy = newy;

        /* The new x and y if we wrap. */
        wx = newx;
        wy = newy;

        if(fx < 1 && fx <= fy) {
            /* If fx < 1 then we will eventually wrap around the left
               or right edge of the screen.
               If fx <= fy then we will wrap around the right or left
               edge of the screen in this iteration. */
            going_left = newx < 0;

            /* x moves to the edge of the screen */
            ix = going_left ? -250.5 : 250.5;

            iy = y + fx * (newy - y); /* y moves by the fraction of x we've
                                         moved. */
            newx += going_left ? 501 : -501; /* If we're moving left, the
                                                target needs to be moved
                                                right by the screen width.
                                                Right is the opposite. */
            wx = going_left ? 250.5 : -250.5; /* If going left, wrap to right
                                                 edge. */
            wy = iy; /* Since y didn't wrap, y intercept is the new
                        y coordinate. */
                                    
        } else if(fy < 1 && fy <= fx) {
            /* Mirror of above code. */
            going_down = newy < 0;
            iy = going_down ? -250.5 : 250.5;
            ix = x + fy * (newx - x);
            newy += going_down ? 501 : -501;
            wy = going_down ? 250.5 : -250.5;
            wx = ix;
        }

        if(pendown) {
            /* Draw a single, non-wrapping segment.
               If the line wraps, this will only draw to the end
               of the screen. */
            draw_line(x, y, ix, iy, pen, drawmode);
        }

        if(mode == TurtleTypes::FENCE) {
            /* In FENCE mode, the turtle stops at the edge of the screen.
               If the destination is off the screen, then (ix, iy) is the
               point along the edge of the screen where the line leaves
               the screen, so that is the turtle's new location. */
            x = ix; y = iy;
            return;
        } else {
            /* After drawing a segment to the edge of the screen,
               we move the turtle to the wrapped location, (wx, wy),
               which will be at the opposite edge of the screen from
               where the turtle ran off the canvas. */
            x = wx; y = wy;
            if(fx == 1 && fy == 1)
                return;
        }
    }
}


/* Draw an arc on the background. */
static void draw_arc(double x, double y, double heading,
                     double radius, double angle,
                     wxPen pen, TurtleTypes::DrawMode drawmode) {
    wxMemoryDC dc;
    dc.SelectObject(*Background);
    dc.SetBrush(*wxTRANSPARENT_BRUSH);
    if(drawmode == TurtleTypes::ERASE) {
        pen.SetColour(BackgroundColour);
    }
    dc.SetPen(pen);

    /* DrawEllipticArc takes arguments describing a rectangle within
       which an arc will be drawn, so we need to convert from a center
       point and a radius to a top left corner and a width and height.
       The last two arguments describe the starting and ending angles,
       which must be converted from Logo angles (positive angles are
       measured clockwise from the positive y axis) to screen angles
       (positive angles are measured counterclockwise from the positive
       x axis). */
    dc.DrawEllipticArc(250 + x - radius, 250 - y - radius,
                       radius*2, radius*2,
                       90 - heading, 90 - heading - angle);
    dc.SelectObject(wxNullBitmap);
}

/* Cause the turtle to print an arc of the given angle and radius,
   clockwise, beginning at the angle the turtle is pointing.
   Handles wrapping by drawing 9 arcs. */
void Turtle::arc(double angle, double radius) {
    if(!pendown)
        return;

    double xoff, yoff;
    if(mode == TurtleTypes::WRAP)
        for(xoff = -501; xoff <= 501; xoff += 501)
            for(yoff = -501; yoff <= 501; yoff += 501)
                draw_arc(x + xoff, y + yoff, heading, radius, angle,
                         pen, drawmode);
    else
        draw_arc(x, y, heading, radius, angle, pen, drawmode);
}

/* Change a turtle's bitmap. */
void Turtle::setbitmap(wxBitmap bitmap, int xoffset, int yoffset) {
    delete tg;
    tg = new BitmapTurtle(bitmap, xoffset, yoffset);
}

/* Save a turtle's bitmap to a file. */
void BitmapTurtle::save(wxString &file) {
    bm.SaveFile(file, wxBITMAP_TYPE_PNG);
}

/* Does this turtle have any pixels meeting criteria1 that are over
   any background pixels meeting criteria2?
   This is where TURTLE_OVER events are handled. */
bool BitmapTurtle::over(PixelCriteria criteria1,
                        PixelCriteria criteria2,
                        wxGraphicsMatrix gm,
                        TurtleTypes::TurtleMode mode) {

    /* Convert the turtle and background bitmaps into wxImage's so we
       can fetch pixel values. */
    wxImage im = bm.ConvertToImage();
    int imheight = im.GetHeight();
    int imwidth = im.GetWidth();

    wxImage bg = Background->ConvertToImage();

    /* Loop over every pixel in the image, testing criteria1. */
    for(int imy = 0; imy < imheight; imy++) {
        for(int imx = 0; imx < imwidth; imx++) {
            if(criteria1.matches(im, imx, imy)) {
                /* Figure out what background pixel is under this
                   turtle pixel. */
                double ty = imy, tx = imx;
                gm.TransformPoint(&tx, &ty);
                if(mode == TurtleTypes::WRAP) {
                    tx = fmod(tx, 501); if(tx < 0) tx += 501;
                    ty = fmod(ty, 501); if(ty < 0) ty += 501;
                }
                int bgx = round(tx), bgy = round(ty);

                /* See if that background pixel matches criteria2. */
                if(criteria2.matches(bg, bgx, bgy))
                    return true;
            }

        }
    }

    return false;
}

/* Is a particular pixel of the turtle over a background pixel that
   meets the criteria?
   This is where TURTLE_XYOVER events are handled. */
bool Turtle::over(int relativex, int relativey, PixelCriteria criteria) {
    wxGraphicsMatrix gm = matrix();

    wxImage bg = Background->ConvertToImage();

    double tx = relativex, ty = -relativey;
    gm.TransformPoint(&tx, &ty);
    if(mode == TurtleTypes::WRAP) {
        tx = fmod(tx, 501); if(tx < 0) tx += 501;
        ty = fmod(ty, 501); if(ty < 0) ty += 501;
    }
    int bgx = round(tx), bgy = round(ty);
    return criteria.matches(bg, bgx, bgy);
}

/* Is a pixel of this turtle matching criteria1 touching a pixel
   of the other turtle that matches criteria2?
   This is where TURTLE_TOUCHING events are handled. */
bool BitmapTurtle::touching(PixelCriteria criteria1,
                            PixelCriteria criteria2,
                            wxGraphicsMatrix gm,
                            Turtle &other,
                            TurtleTypes::TurtleMode mode) {
    CollisionTester tester(other, criteria2);

    wxImage im = bm.ConvertToImage();
    int imheight = im.GetHeight();
    int imwidth = im.GetWidth();

    for(int imy = 0; imy < imheight; imy++) {
        for(int imx = 0; imx < imwidth; imx++) {
            if(criteria1.matches(im, imx, imy)) {
                /* Transform the coordinates for this turtle into screen
                   coordinates, wrapping if necessary. */
                double ty = imy, tx = imx;
                gm.TransformPoint(&tx, &ty);
                if(mode == TurtleTypes::WRAP) {
                    tx = fmod(tx, 501); if(tx < 0) tx += 501;
                    ty = fmod(ty, 501); if(ty < 0) ty += 501;
                }
                if(tester.CollidesWith(tx, ty))
                    return true;
            }

        }
    }
    return false;
}

/* Is the pixel at the turtle relative coordinates (relativex, relativey)
   touching a nontransparent pixel of the other turtle?
   This is where TURTLE_XYTOUCHIG events are handled. */
bool Turtle::touching(int relativex, int relativey, Turtle &other) {
    wxGraphicsMatrix gm = matrix();
    CollisionTester tester(other, PixelCriteria(NONTRANSPARENT));

    double tx = relativex, ty = -relativey;
    gm.TransformPoint(&tx, &ty);
    if(mode == TurtleTypes::WRAP) {
        tx = fmod(tx, 501); if(tx < 0) tx += 501;
        ty = fmod(ty, 501); if(ty < 0) ty += 501;
    }
    return tester.CollidesWith(tx, ty);
}

/* Does my pixel at turtle relative coordinate (x, y) match the
   criteria? */
bool BitmapTurtle::matches(PixelCriteria criteria, int x, int y) {
    wxImage im = bm.ConvertToImage();
    return criteria.matches(im, x, y);
}

/* For PathTurtles, we ignore criteria1 and just trace the edge of the
   triangle. */
bool PathTurtle::touching(PixelCriteria criteria1,
                          PixelCriteria criteria2,
                          wxGraphicsMatrix gm,
                          class Turtle &other,
                          TurtleTypes::TurtleMode mode) {
    CollisionTester tester(other, criteria2);

    /* We're going to run x from -6 to 6.  As we do, we trace both the
       line from (-6, 0) to (6, 0) and the lines from (-6, 0) to (0, -19)
       and from (0, -19) to (6, 0).  We are in wxWidgets coordinates
       here, so the positive y axis is down. */
    double d = -6;
    while(d <= 6) {
        /* Test the point along the line from (-6, 0) to (6, 0),
           defined by "y = 0". */
        double tx = d, ty = 0;
        /* Transform the point into screen coordinates. */
        gm.TransformPoint(&tx, &ty);
        if(mode == TurtleTypes::WRAP) {
            tx = fmod(tx, 501); if(tx < 0) tx += 501;
            ty = fmod(ty, 501); if(ty < 0) ty += 501;
        }
        if(tester.CollidesWith(tx, ty))
            return true;

        /* Test a point along one of the sides of the triangle.
           If d < 0 then we are testing the line from (-6, 0) to
           (0, -19).  If d >= 0 then we are testing the line from
           (0, -19) to (6, 0). */
        tx = d; ty = d < 0 ? -(19+d*19/6) : -(19-d*19/6);
        /* Transform into screen coordinates. */
        gm.TransformPoint(&tx, &ty);
        if(mode == TurtleTypes::WRAP) {
            tx = fmod(tx, 501); if(tx < 0) tx += 501;
            ty = fmod(ty, 501); if(ty < 0) ty += 501;
        }
        if(tester.CollidesWith(tx, ty))
            return true;
        d += 1;
    }

    return false;
}

/* Is a path turtle over any background colors that match criteria2?
   The strategy is the same as PathTurtle::touching above, except we
   test the background and not another turtle. */
bool PathTurtle::over(PixelCriteria criteria1,
                        PixelCriteria criteria2,
                        wxGraphicsMatrix gm,
                        TurtleTypes::TurtleMode mode) {
    wxImage bg = Background->ConvertToImage();

    /* We're going to run x from -6 to 6.  As we do, we trace both the
       line from (-6, 0) to (6, 0) and the lines from (-6, 0) to (0, -19)
       and from (0, -19) to (6, 0).  We are in wxWidgets coordinates
       here, so the positive y axis is down. */
    double d = -6;
    while(d <= 6) {
        /* Test the point along the line from (-6, 0) to (6, 0),
           defined by "y = 0". */
        double tx = d, ty = 0;
        /* Transform the point into screen coordinates. */
        gm.TransformPoint(&tx, &ty);
        if(mode == TurtleTypes::WRAP) {
            tx = fmod(tx, 501); if(tx < 0) tx += 501;
            ty = fmod(ty, 501); if(ty < 0) ty += 501;
        }
        int bgx = round(tx), bgy = round(ty);
        /* Test the identified background pixel. */
        if(criteria2.matches(bg, bgx, bgy))
            return true;

        /* Test a point along one of the sides of the triangle.
           If d < 0 then we are testing the line from (-6, 0) to
           (0, -19).  If d >= 0 then we are testing the line from
           (0, -19) to (6, 0). */
        tx = d; ty = d < 0 ? -(19+d*19/6) : -(19-d*19/6);
        /* Transform into screen coordinates. */
        gm.TransformPoint(&tx, &ty);
        if(mode == TurtleTypes::WRAP) {
            tx = fmod(tx, 501); if(tx < 0) tx += 501;
            ty = fmod(ty, 501); if(ty < 0) ty += 501;
        }
        bgx = round(tx); bgy = round(ty);
        /* Test the identified background pixel. */
        if(criteria2.matches(bg, bgx, bgy))
            return true;
        d += 1;
    }

    return false;
}

/* Fill the contiguous region of the screen whose color matches the
   pixel under the turtle with the turtle's pen color. */
void Turtle::fill() {
    /* Figure out which background pixel we're over. */
    int bgx = 250 + round(x);
    int bgy = 250 - round(y);
    int maxx = Background->GetWidth();
    int maxy = Background->GetHeight();

    /* Do nothing if we're off the screen. */
    if(bgx < 0 || bgx >= maxx || bgy < 0 || bgy >= maxy)
        return;

    /* Convert the background to a wxImage so we can examine its
       pixels. */
    wxImage im = Background->ConvertToImage();

    /* Fetch the color of the pixel under the turtle.
       We need to fetch the three components separately and
       construct a new exColour object. */
    wxColour c(im.GetRed(bgx, bgy),
               im.GetGreen(bgx, bgy),
               im.GetBlue(bgx, bgy));

    wxMemoryDC dc;
    dc.SelectObject(*Background);
    /* wxDC::FloodFill() uses the brush, not the pen, so we need to
       set the brush color to match the current pen color. */
    dc.SetBrush(pen.GetColour());

    /* wxFLOODD_SURFACE means paint using the current brush (which we
       just set to the current color of the turtle's pen) on the pixel
       at (bgx, bgy), and all pixels in a contiguous region containing
       it, that currently have color c. */
    dc.FloodFill(bgx, bgy, c, wxFLOOD_SURFACE);
    dc.SelectObject(wxNullBitmap);
}

/* Print text on the background at the location of the turtle.
   The text is not rotated. */
void Turtle::label(wxString &s) {
    /* Convert Logo coordinates to screen coordinates. */
    int bgx = 250 + round(x);
    int bgy = 250 - round(y);

    wxMemoryDC dc;
    dc.SelectObject(*Background);

    /* Set the background and forground for the text to print. */
    dc.SetBackgroundMode(wxTRANSPARENT);
    dc.SetTextForeground(pen.GetColour());

    /* Use the system default font. */
    dc.SetFont(*wxNORMAL_FONT);

    /* wxWidgets prints text below and to the right of the given location.
       ucblogo prints text above and to the right of the given location.
       To print the text above the turtle's location, we calculate the
       height of the text and raise the y coordinate by that far. */
    wxSize size = dc.GetMultiLineTextExtent(s);
    bgy -= size.GetHeight();
    dc.DrawText(s, bgx, bgy);
    dc.SelectObject(wxNullBitmap);
}

