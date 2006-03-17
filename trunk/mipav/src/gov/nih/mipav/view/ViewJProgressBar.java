package gov.nih.mipav.view;

import java.awt.*;
import java.awt.event.*;
import java.io.FileNotFoundException;
import javax.swing.*;

/**
 * Progress bar used everywhere for displaying to the user how long the current
 * process is going to take. The progress bar looks like this:
 * 
 * <pre>
 * 		+-Title------------------------+
 * 		| Message                      |
 * 		| |--------Bar--------|	percent|
 * 		|           Cancel             |
 * 		+------------------------------+
 * 		
 * </pre>
 * 
 * The cancel button is not always present; that option is set in the
 * constructor.
 * 
 * @version 0.1 Oct 19, 1998
 * @author Matthew J. McAuliffe, Ph.D.
 * 
 */
public class ViewJProgressBar extends JFrame implements ActionListener,
        ProgressBarInterface {

    /**
     * Actual bar which fills with color as the percentage of completion
     * increases.
     */
    private JProgressBar pBar;

    /**
     * Percentage label, puts a number next to the bar with the actual percent
     * completed.
     */
    private JLabel percentage;

    /**
     * Label directly above the bar that is updated during operation if
     * necessary.
     */
    private JLabel messagePBar;

    /** Cancel button, if applicable. */
    private JButton cancelButton;

    /** Title of the frame for the progress bar. */
    private String title;

    private boolean separateThread;

    /**
     * Creates a new progress bar with the given title, message, and min and
     * max. The percentage is initially set to "0%". The <code>cancelFlag</code>
     * indicates if there should be a cancel button - usually this is used for
     * algorithms, which can be stopped, but not loading images.
     * 
     * @param _title
     *            Title for the frame of the progress bar.
     * @param msg
     *            Message directly above the actual progress bar.
     * @param min
     *            Minimum value progress bar takes.
     * @param max
     *            Maximum value progress bar takes.
     * @param cancelFlag
     *            <code>true</code> indicates that the user can stop the
     *            process by clicking on the cancel button, and so the button
     *            should be added to the frame.
     * @param actionListener
     *            Listener to tie to cancel button.
     * @param windowListener
     *            Listener to tie to this frame.
     */
    public ViewJProgressBar(String _title, String msg, int min, int max,
            boolean cancelFlag, ActionListener actionListener,
            WindowListener windowListener) {
        super(_title);

        title = _title;

        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <"
                    + error.getMessage()
                    + ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <"
                    + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }

        cancelButton = new JButton("Cancel");

        pBar = new JProgressBar();
        pBar.setMinimum(min);
        pBar.setMaximum(max);
        pBar.setValue(0);
        pBar.setPreferredSize(new Dimension(220, 20));
        pBar.setAlignmentX(JComponent.LEFT_ALIGNMENT);

        messagePBar = new JLabel(msg);
        messagePBar.setForeground(Color.black);
        messagePBar.setFont(MipavUtil.font12B);
        messagePBar.setAlignmentX(JComponent.LEFT_ALIGNMENT);

        // prototype value is 100%, so that packing doesn't make the size of the
        // label too small.
        percentage = new JLabel("1000%");
        percentage.setFont(MipavUtil.font12B);
        percentage.setPreferredSize(new Dimension(50, 30));

        if (cancelFlag) {
            JPanel buttonPanel = new JPanel();
            if (actionListener != null) {
                cancelButton.addActionListener(actionListener);
            } else {
                cancelButton.addActionListener(this);
            }
            cancelButton.setToolTipText("Stop process");
            cancelButton.setMnemonic('d');
            cancelButton.setFont(MipavUtil.font12B);
            cancelButton.setPreferredSize(new Dimension(90, 30));
            cancelButton.setActionCommand("cancel");
            buttonPanel.add(cancelButton);
            getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        }
        JPanel labelPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);
        labelPanel.add(messagePBar, gbc);

        getContentPane().add(labelPanel, BorderLayout.NORTH);

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        mainPanel.add(pBar);
        mainPanel.add(percentage);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        if (windowListener != null) {
            addWindowListener(windowListener);
        } else {
            // Note: this doesn't get triggered when called from the same Thread
            setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        }
        pack();
        MipavUtil.centerOnScreen(this);
        percentage.setText("0%");

    }

    /**
     * Changes whether the algorithm which is using this progress bar is running
     * in its own thread.
     * 
     * @param flag
     *            whether the progress bar's algorithm is running is a separate
     *            thread
     */
    public void setSeparateThread(boolean flag) {
        separateThread = flag;
    }

    /**
     * Use this method if you are not running in a separate thread, or, you have
     * already set the value of separateThread through the setSeparateThread
     * method. Otherwise, use updateValue(int, boolean);
     * 
     * @param value
     *            new progress bar value
     */
    public void updateValue(int value) {
        pBar.setValue(value);
        percentage.setText(String.valueOf(value) + "%");
        super.setTitle(title + "   " + String.valueOf(value) + "%");

        /*
         * For some reason, this bottom three lines now suddenly make the
         * progressbar flicker
         */
        if (!this.separateThread) {
            update(this.getGraphics());
        }
    }

    /**
     * Used to get the present value of the progress bar.
     * 
     * @return Value of progress bar.
     */
    public int getValue() {
        return pBar.getValue();
    }

    /**
     * Get the progress bar.
     * 
     * @return pBar the progress bar.
     */
    public JProgressBar getProgressBar() {
        return pBar;
    }

    /**
     * Get the percentage label.
     * 
     * @return percentage percentage label.
     */
    public JLabel getPercentageLabel() {
        return percentage;
    }

    /**
     * Get the message label
     * 
     * @return messagePBar message label.
     */
    public JLabel getMessageBar() {
        return messagePBar;
    }

    /**
     * Used to determine if the progress bar is at 100%.
     * 
     * @return <code>true</code> if progress bar is at 100%,
     *         <code>false</code> if not.
     */
    public boolean isComplete() {
        return (pBar.getPercentComplete() == 1);
    }

    /**
     * Used to set the present value of the progress bar. Changes the percentage
     * label and title of the frame as well.
     * 
     * @param value
     *            Set the progress bar to the given value.
     * @param sepThread
     *            Whether the calling algorithm is running in a separate thread 
     */
    public void updateValue(int value, boolean sepThread) {
        pBar.setValue(value);
        percentage.setText(String.valueOf(value) + "%");
        super.setTitle(title + "   " + String.valueOf(value) + "%");

        /*
         * For some reason, this bottom three lines now suddenly make the
         * progressbar flicker
         */
        if (!sepThread) {
            update(this.getGraphics());
        }
    }

    /**
     * Used to set the present value of the progress bar. Changes the percentage
     * label and title of the frame as well. It also forces an immediate update
     * of the frame. Typically used when using IO functions.
     * 
     * @param value
     *            Set the progress bar to the given value.
     */
    public void updateValueImmed(int value) {
        pBar.setValue(value);
        percentage.setText(String.valueOf(value) + "%");
        super.setTitle(title + "   " + String.valueOf(value) + "%");
        update(this.getGraphics());
    }

    /**
     * Enable or disable the progress bar indeterminate mode
     * 
     * @param flag
     *            boolean
     */
    public void setIndeterminate(boolean flag) {
        pBar.setIndeterminate(flag);
    }

    /**
     * Sets the message area of the progress bar.
     * 
     * @param msg
     *            Message to be displayed.
     */
    public void setMessage(String msg) {
        messagePBar.setText(msg);
    }

    /**
     * Sets the title of the frame of the progress bar.
     * 
     * @param _title
     *            Title of the frame.
     */
    public void setTitle(String _title) {
        title = _title;
    }

    /**
     * Concatenates a message to the message area of the progress bar. Be sure
     * to provide appropriate leading spacing.
     * 
     * @param msg
     *            Message to be displayed.
     */
    public void appendMessage(String msg) {
        String text = messagePBar.getText();
        messagePBar.setText(text + msg);
    }

    /**
     * Closes this progress bar when the cancel button is clicked. This doesn't
     * actually get called when the progress bar is running in the same thread.
     * 
     * @param e
     *            Event that triggered this method.
     */
    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == cancelButton) {
            dispose();
        }
    }
}
