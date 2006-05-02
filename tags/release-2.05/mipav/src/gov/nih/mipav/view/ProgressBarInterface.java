package gov.nih.mipav.view;


import javax.swing.*;


/**
 * <p>Title: ProgressBarInterface</p>
 *
 * <p>Description: Interface for accessing progress bar functions (within either JComponents such as JPanel or
 * JFrame</p>
 *
 * @author   sir benjamin link
 * @version  .99a
 */

public interface ProgressBarInterface {
    // public void setValue( int value );

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  msg  DOCUMENT ME!
     */
    void appendMessage(String msg);

    /**
     * DOCUMENT ME!
     */
    void dispose();

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    JProgressBar getProgressBar();

    /**
     * public void setValue( int value, boolean separateThread ); public void setValueImmed( int value );
     *
     * @return  DOCUMENT ME!
     */
    int getValue();

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    boolean isComplete();

    /**
     * DOCUMENT ME!
     *
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     */
    void setLocation(int x, int y);

    /**
     * public void updateValueImmed( int value );
     *
     * @param  msg  DOCUMENT ME!
     */
    void setMessage(String msg);

    /**
     * DOCUMENT ME!
     *
     * @param  _title  DOCUMENT ME!
     */
    void setTitle(String _title);

    /**
     * DOCUMENT ME!
     *
     * @param  blah  DOCUMENT ME!
     */
    void setVisible(boolean blah);

    /**
     * public void updateValue( int value );
     *
     * @param  value           DOCUMENT ME!
     * @param  separateThread  DOCUMENT ME!
     */
    void updateValue(int value, boolean separateThread);
}
;
