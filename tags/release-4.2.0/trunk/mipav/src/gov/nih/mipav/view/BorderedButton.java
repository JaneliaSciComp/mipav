package gov.nih.mipav.view;


import java.awt.*;

import javax.swing.*;


/**
 * This class was created for use in the JDialogMultiPaint class. It is intended to show a border around a button to
 * indicate that it is 'selected' or 'highlighted'.
 *
 * @author  orsinol
 */
public class BorderedButton extends JButton {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5566213886996905888L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Color borderColor = Color.yellow;

    /** DOCUMENT ME! */
    private boolean borderOn = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new BorderedButton object.
     *
     * @param  text  DOCUMENT ME!
     */
    public BorderedButton(String text) {
        super(text);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Color getBorderColor() {
        return borderColor;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isBorderOn() {
        return borderOn;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  graphics  DOCUMENT ME!
     */
    public void paintComponent(Graphics graphics) {
        super.paintComponent(graphics);

        if (borderOn) {
            graphics.setColor(borderColor);
            graphics.drawRect(2, 2, getSize().width - 5, getSize().height - 5);
            graphics.drawRect(3, 3, getSize().width - 7, getSize().height - 7);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  newBorderColor  DOCUMENT ME!
     */
    public void setBorderColor(Color newBorderColor) {
        borderColor = newBorderColor;

        repaint();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  state  DOCUMENT ME!
     */
    public void setBorderOn(boolean state) {
        borderOn = state;

        repaint();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  selected  DOCUMENT ME!
     */
    public void setSelected(boolean selected) {
        super.setSelected(selected);

        setBorderOn(selected);
    }

}
