package gov.nih.mipav.view;

import java.awt.*;
import javax.swing.*;

// imports for memory monitor
import java.awt.image.BufferedImage;
import java.awt.geom.Rectangle2D;


/**
 *  BarMeter presents a vertical, block-style meter to present numerical
 *  information as a fraction of a number of divisions (eg, progress or
 *  resource consumption).  The default number of divisions is variable, but
 *  the default is 1.  The meter represents inputted values as one of either
 *  color, one "Used" and the other "Un-Used."  Used are filled in to the
 *  the percentage value inputted (integer) divided by the number of divisions.
 *  The default color used is Green, and default unused is Dark Grey.
 *
 *
 *       $Logfile: /mipav/src/gov/nih/mipav/view/BarMeter.java $
 *       $Revision: 10 $
 *       $Date: 7/02/04 1:30p $
 *
 */
public class BarMeter   extends JPanel
{
    private int amplitude = 0;
    private int divisions = 1;


    Rectangle2D unusedRect = new Rectangle2D.Float();
    Rectangle2D usedRect   = new Rectangle2D.Float();
    int w = 0;
    int h = 0;                           // width, height
    int boxHeight;
    int boxWidth;
    int whitespaceH = 10;
    int whitespaceW = 10;
    Graphics2D g2d;
    BufferedImage img;

    Color usedColor   = Color.cyan;
    Color unusedColor = Color.darkGray;

    /**
     *  Constructs a barmeter with a background color of black.  Does not
     *  show the meter.
     */
    public BarMeter() {
        super();
        setBackground(Color.black);
    }

    /** sets the height of the of the bar reading.
    *   @param amp  percentage amount of the amplitude.
    *               ARguments greater than 100 (&#37) or less than
    *               zero are thrown out.
    */
    public void setAmplitude(int amp) {
        if (amp < 0 || amp > 100) {
            return;
        }
        else {
            amplitude = divisions - (int)(amp/100.0f * divisions);
        }
    }

    /** applies the given int to the total number of bars on the display
    *   @param div  sets the number of bars, or divisions, on the display.
    *               the finest resolution that may be displayed, is then
    *               100/div (&#37).
    */
    public void setDivisions(int div) {
        if (div < 1) {
            return;
        }
        divisions = div;
    }

    /** applies the given color to bars which are "lit" and are up
    *   to the set amplitude
    *   @param c    color used in divisions that represent values
    *               smaller than the amplitude.
    */
    public void setUsedColor(Color c) {
        usedColor = c;
    }

    /** applies the given color to bars which are not lit and are
    *   above the set amplitude
    *   @param c    color used in divisions that represent values
    *               smaller than the amplitude.
    */
    public void setUnusedColor(Color c) {
        unusedColor = c;
    }

    /** Minimum size of this panel */
    public Dimension getMinimumSize() {
        return getPreferredSize();
    }

    /** Maximum size of this panel */
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }

    /**
    *   Preferred size of this panel
    */
    public Dimension getPreferredSize() {
         return new Dimension(30, 120);
    }

    /** Draws the vertical bar, or all 100&#37 of posible amplitude values*/
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        g2d = (Graphics2D) g;

        Insets in = getInsets();
        Dimension d = getSize();
        if (d.width != w || d.height != h) {
            w = d.width - in.left - in.right;
            h = d.height - in.top - in.bottom;
            boxHeight = (h - 2*whitespaceH)/divisions;
            boxWidth  = w - 2*whitespaceW;
        }

        // .. unused ..
        g2d.setColor(unusedColor);
        int i;
        for (i = 0; i < amplitude; i++) {
            unusedRect.setRect(whitespaceW + in.left,
                               (float) whitespaceH + i*boxHeight + in.top,
                               boxWidth,
                               (float) boxHeight-1);
            g2d.fill(unusedRect);
        }

        // .. used ..
        g2d.setColor(usedColor);
        for( ; i < divisions; i++)  { // takes off where the empty boxes left off
            usedRect.setRect(whitespaceW + in.left,
                             (float) whitespaceH + i*boxHeight + in.top,
                             boxWidth,
                             (float) boxHeight-1);
            g2d.fill(usedRect);
        }

    }
}
