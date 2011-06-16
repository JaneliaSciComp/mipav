package gov.nih.mipav.view;


import java.awt.*;
import java.awt.event.*;
import java.text.DecimalFormat;

import javax.swing.*;

public class ViewJComponentGraphAxes extends JComponent {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4064300190247654824L;

    public static final int X_AXIS = 0;
    public static final int Y_AXIS = 1;
    public static final int LEFT = 0;
    public static final int RIGHT = 1;
    public static final int TOP = 0;
    public static final int BOTTOM = 1;

    /** DOCUMENT ME! */
    private Rectangle bounds;

    /** DOCUMENT ME! */
    private Font font12B;

    /** DOCUMENT ME! */
    private int gridLines = -1;

    /** DOCUMENT ME! */
    private String label = null;

    private int orientation = X_AXIS;
    private int justified = TOP;
    private int border = 0;
    protected float min, max;
    
    public ViewJComponentGraphAxes( int orientation, int justified, int width, int height, String label, int border ) {
    	bounds = new Rectangle(0, 0, width, height);
    	this.label = label;
    	this.orientation = orientation;
    	this.justified = justified;
    	this.border = border;
    	this.min = 0;
    	this.max = Math.max(width,height);
        setBounds(bounds);
        setVisible(true);
    }
    
    public void setMinMax( float min, float max )
    {
    	this.min = min;
    	this.max = max;
    }

    /**
     * Accessor that returns the preferred size of the component.
     *
     * @return  the preferred size
     */
    public Dimension getPreferredSize() {
        Dimension dim;

        try {
            dim = new Dimension(bounds.width, bounds.height);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.getPreferredSize");

            return null;
        }

        return dim;
    }

    /**
     * Paints the graph. Checks if the labels are defined and draws them, checks if the gridlines should be shown and
     * draws them, then calls PolyLine with the points.
     *
     * @param  g  Graphics to paint in
     */
    public void paintComponent(Graphics g) {

        if (g == null) {
            return;
        }
        DecimalFormat df = new DecimalFormat();
        df.setMaximumFractionDigits(2);
        df.setMinimumFractionDigits(0);
        
        if ( orientation == X_AXIS && (this.label != null) )
        {
        	// Draw the label for the x axis
        	if (label != null) {
        		g.setFont(font12B);
        		g.setColor(Color.black);
        		g.drawString(label, ((bounds.width / 2) - (g.getFontMetrics().stringWidth(label)/2)), bounds.height - 5);
        	}

            // Distance along the axis for the tick marks and gridlines
        	gridLines = 4;
        	double xTick = (bounds.width - this.border*2) / ((double) gridLines);
        	double labelTick = (max - min)/(double)gridLines;
            //yTick = graphBounds.height / ((double) yGridLines);

            // Draw tick marks
            for (int i = 0; i <= gridLines; i++) {
                g.setColor(Color.black);
                g.drawLine(bounds.x + this.border + (int) Math.round(i * xTick), 0,
                		bounds.x + this.border + (int) Math.round(i * xTick), + 6);
                g.setColor(Color.blue);
                String label = df.format(min + i * labelTick);
                g.drawString(label, bounds.x + border + (int) Math.round(i * xTick) - (g.getFontMetrics().stringWidth(label)/2), 20);
            }
            
            for (int i = 1; i < (gridLines * 10); i++) {
                g.setColor(Color.black);
                g.drawLine(bounds.x + this.border + (int) Math.round(i * (xTick / 10)), 0,
                		bounds.x + this.border + (int) Math.round(i * (xTick / 10)), 3);
            }
        }
        else if ( orientation == Y_AXIS && (this.label != null) )
        {
            // Draw the label for the y axis
            int fontSize = g.getFont().getSize();
            if (label != null) {
                g.setFont(font12B);
                g.setColor(Color.black);
                int halfHeight = fontSize * label.length() / 2;

                for (int i = 0; i < label.length(); i++) {
                    g.drawString(String.valueOf(label.charAt(i)), bounds.width - fontSize - g.getFontMetrics().charWidth(label.charAt(i))/2, ((bounds.height / 2) - halfHeight + i*fontSize));
                }
            }

        	gridLines = 4;
            double yTick = bounds.height / ((double) gridLines);
        	double labelTick = (max - min)/(double)gridLines;
            for (int i = gridLines; i >= 0; i--) {
            	g.setColor(Color.black);
            	g.drawLine(bounds.x, bounds.y + (int) Math.round(i * yTick), bounds.x + 6,
            			bounds.y + (int) Math.round(i * yTick));
                g.setColor(Color.blue);
                String label = df.format(min + (gridLines - i) * labelTick);
                if ( i == 0 )
                {
                	g.drawString(label, bounds.x + 10, bounds.y + fontSize + (int) Math.round(i * yTick));
                }
                else
                {
                	g.drawString(label, bounds.x + 10, bounds.y + (int) Math.round(i * yTick));
                }
            }

            // Draw minor tick marks
            for (int i = (gridLines * 10) - 1; i > 0; i--) {
            	g.setColor(Color.black);
            	g.drawLine(bounds.x, bounds.y + (int) Math.round(i * (yTick / 10)), bounds.x + 3,
            			bounds.y + (int) Math.round(i * (yTick / 10)));
            }

        }


    }



    /**
     * Calls paint.
     *
     * @param  g  Graphics to paint in
     */
    public void update(Graphics g) {
        paintComponent(g);
    }


    /**
     * Makes a string of a float with a specific number of decimal points.
     *
     * @param   number  number to be converted to a string
     * @param   decPts  the number of decimal points
     *
     * @return  string representation of the number
     */
    private String makeString(float number, int decPts) {
        String subStr = null;
        String str = null;

        try {
            subStr = new String();
            str = new String(String.valueOf(number));
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("FrameBase.makeString: out of memory");

            return null;
        }

        int index = str.indexOf(".");
        int length = str.length();

        if ((index + decPts) < length) {
            subStr = str.substring(0, index + decPts + 1);
        } else {
            subStr = str;
        }

        return subStr;
    }

}
