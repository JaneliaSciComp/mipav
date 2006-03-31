package gov.nih.mipav.view;


import javax.swing.*;


/**
 * <p>Title: ProgressBarInterface</p>
 * <p>Description: Interface for accessing progress bar functions (within either JComponents such
 * as JPanel or JFrame</p>
 * @author sir benjamin link
 * @version .99a
 */

public interface ProgressBarInterface {
   // public void setValue( int value );

   // public void setValue( int value, boolean separateThread );

   // public void setValueImmed( int value );

    public int getValue();

    public JProgressBar getProgressBar();

    public boolean isComplete();

    //public void updateValue( int value );

    public void updateValue( int value, boolean separateThread );

    //public void updateValueImmed( int value );

    public void setMessage( String msg );

    public void setTitle( String _title );

    public void appendMessage( String msg );

    public void setVisible( boolean blah );

    public void setLocation(int x, int y);

    public void dispose();
}


;
