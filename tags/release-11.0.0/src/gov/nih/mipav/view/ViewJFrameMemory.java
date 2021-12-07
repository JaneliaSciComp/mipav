package gov.nih.mipav.view;


import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.actions.*;
import gov.nih.mipav.model.provenance.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;
import java.lang.management.ManagementFactory;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;


/**
 * Contains a seperatly running thread which checks the currently used and available memory. This frame will display
 * that information several different ways:
 *
 * <ul>
 *   <li>in a text-area displaying memory used in K, total memory available to the JVM in K, and the relative used
 *     amount in per-cent.</li>
 *   <li>in a bar-chart that resembles a graphic equaliser on a stereo.</li>
 *   <li>in line-graph providing history information.</li>
 * </ul>
 *
 * <p>The memory usage is sampled at a rate which is user-specified. The frame will allow the user to adjust how
 * frequently the memory size will be sampled. For efficiency, no sampling will occur if the frame has been minimized.
 * </p>
 *
 * <p>The memory frame provides direct user access to the function Runtime.getRuntime.gc() via a "Garbage Collector"
 * button.</p>
 *
 * @version  0.2 16 June 2001
 * @author   David Parsons
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class ViewJFrameMemory extends JFrame implements ActionListener, ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8863264033308071151L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private BarMeter bm;

    /** DOCUMENT ME! */
    private JButton callGCbutton;

    /** DOCUMENT ME! */
    private LineMeter lm;

    /** DOCUMENT ME! */
    private JButton pauseButton;

    /** DOCUMENT ME! */
    private boolean paused = false; // don't sample memory when true

    /** DOCUMENT ME! */
    private JLabel percentUsed;

    /** DOCUMENT ME! */
    private JTextField sampleRate; // user control over memory sampling period

    /** thread. */
    private MemoryMonitor surf;

    /** DOCUMENT ME! */
    private JLabel total;

    /** test notification of memory used. */
    private JLabel used;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     */
    public ViewJFrameMemory() {
        super();

        setTitle("Memory Monitor");

        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        // don't waste time or cycles checking memory when the window isn't
        // visible:
        this.addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent we) {
                    surf.stop();
                    dispose();
                }

                public void windowIconified(WindowEvent we) {
                    paused = false;
                    pauseButton.doClick();
                }

                public void windowDeiconified(WindowEvent we) {
                    paused = true;
                    pauseButton.doClick();
                }
            });
        this.getContentPane().setLayout(new BorderLayout());


        TitledBorder border;

        JPanel userPanel = new JPanel(new BorderLayout());

        // panel is titled & etched
        JPanel setupPanel = new JPanel(new BorderLayout());
        border = new TitledBorder("Sampling");
        border.setTitleColor(Color.black);
        border.setTitleFont(MipavUtil.font12B);
        border.setBorder(new EtchedBorder());
        setupPanel.setBorder(border);

        pauseButton = new JButton("Pause");
        pauseButton.setFont(MipavUtil.font12B);
        pauseButton.setPreferredSize(new Dimension(100, 30));
        pauseButton.addActionListener(this);
        setupPanel.add(pauseButton, BorderLayout.NORTH);

        JPanel samplePanel = new JPanel();
        JLabel labelSampleRate = new JLabel("Sample Rate"); // add name for user input
        labelSampleRate.setFont(MipavUtil.font12);
        labelSampleRate.setBackground(Color.black);
        samplePanel.add(labelSampleRate);
        samplePanel.add(Box.createHorizontalStrut(10)); // add spacing
        sampleRate = new JTextField("1000"); // add user input field
        makeNumericsOnly(sampleRate); // we cannot yet call MipavUtil here....
        sampleRate.setColumns(5);
        sampleRate.setHorizontalAlignment(JTextField.RIGHT);
        sampleRate.addActionListener(this);
        samplePanel.add(sampleRate);

        JLabel ms = new JLabel("ms"); // add sample rate unit
        ms.setFont(MipavUtil.font12);
        ms.setBackground(Color.black);
        samplePanel.add(ms);
        setupPanel.add(samplePanel, BorderLayout.SOUTH);

        userPanel.add(setupPanel, BorderLayout.NORTH);

        // panel gets a grid layout
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 4, 2, 4); // component width = minwidth + (2ipadx)

        JPanel dataPanel = new JPanel();
        dataPanel.setLayout(gbl);
        border = new TitledBorder("Current State");
        border.setTitleColor(Color.black);
        border.setTitleFont(MipavUtil.font12B);
        border.setBorder(new EtchedBorder());
        dataPanel.setBorder(border);


        long inUse = MipavUtil.getUsedHeapMemory() / 1024;
        long tot = MipavUtil.getMaxHeapMemory() / 1024;

        JLabel k = new JLabel("K"); // units for memory usage
        JLabel K = new JLabel("K");
        JLabel cent = new JLabel("%");
        k.setFont(MipavUtil.font12);
        K.setFont(MipavUtil.font12);
        cent.setFont(MipavUtil.font12);


        JLabel usedLabel = new JLabel("Used:");
        usedLabel.setFont(MipavUtil.font12);
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(usedLabel, gbc);
        dataPanel.add(usedLabel);

        used = new JLabel(Long.toString(inUse)); // inUse (k) =
        used.setFont(MipavUtil.font12);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(used, gbc);
        dataPanel.add(used);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(k, gbc);
        dataPanel.add(k);

        JLabel totalLabel = new JLabel("Total:");
        totalLabel.setFont(MipavUtil.font12);
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(totalLabel, gbc);
        dataPanel.add(totalLabel);

        total = new JLabel(Long.toString(tot));
        total.setFont(MipavUtil.font12);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(total, gbc);
        dataPanel.add(total);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(K, gbc);
        dataPanel.add(K);


        JLabel percentLabel = new JLabel("Percent Used:");
        percentLabel.setFont(MipavUtil.font12);
        gbc.gridwidth = 2;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(percentLabel, gbc);
        dataPanel.add(percentLabel);

        percentUsed = new JLabel(Integer.toString(Math.round((float) (inUse) / tot * 100)));
        percentUsed.setFont(MipavUtil.font12);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        gbl.setConstraints(percentUsed, gbc);
        dataPanel.add(percentUsed);

        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.anchor = GridBagConstraints.WEST;
        gbl.setConstraints(cent, gbc);
        dataPanel.add(cent);

        userPanel.add(dataPanel, BorderLayout.CENTER);

        border = new TitledBorder("Sampling");
        border.setTitleColor(Color.black);
        border.setTitleFont(MipavUtil.font12B);
        border.setBorder(new EtchedBorder());
        setupPanel.setBorder(border);

        bm = new BarMeter();
        bm.setDivisions(20);
        userPanel.add(bm, BorderLayout.EAST);

        callGCbutton = new JButton("Free memory");
        callGCbutton.setFont(MipavUtil.font12B);
        callGCbutton.setPreferredSize(new Dimension(100, 30));
        callGCbutton.addActionListener(this);
        userPanel.add(callGCbutton, BorderLayout.SOUTH);

        this.getContentPane().add(userPanel, BorderLayout.WEST);

        surf = new MemoryMonitor();
        surf.addMemoryChangeListener(this);
        sampleRate.addFocusListener(surf);

        // make a panel to show the history graph
        JPanel barf = new JPanel(new BorderLayout());
        barf.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        lm = new LineMeter();
        lm.setSampleRate(1000);
        barf.add(lm);
        lm.start();

        // make a panel to show the equaliser
        JPanel bord = new JPanel(new BorderLayout());
        bord.setBorder(BorderFactory.createEmptyBorder(10, 0, 10, 10));
        bord.add(bm);
        this.getContentPane().add(barf, BorderLayout.CENTER);
        this.getContentPane().add(bord, BorderLayout.EAST);

        // start the memory checker
        surf.start();
        pack();
        setVisible(true);
        callGCbutton.requestFocus();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods based on the user's actions.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == pauseButton) {

            if (paused) { // if already paused.... change the sample rate, and unpause
                surf.start();
                lm.setSampleRate(Integer.parseInt((sampleRate).getText()));
                lm.start();
                paused = false;
                pauseButton.setText("Pause");
                validate();
            } else { // not paused?  stop checking the memory, doing updates and tell user
                surf.stop();
                lm.stop();
                paused = true;
                pauseButton.setText("Resume");
                validate();
            }
        } else if (source == sampleRate) { }
        else if (source == callGCbutton) { // call the garbage collector
            Runtime.getRuntime().gc();
            // Runtime.getRuntime().runFinalization();
            if (paused) {
                surf.stop(); // should update the display & memory values
                validate();
                repaint();
            }

            ProvenanceRecorder.getReference().addLine(new ActionCollectGarbage());
            ScriptRecorder.getReference().addLine(new ActionCollectGarbage());
        }
    }
    // ************************************************************************
    // **************************** Change Events *****************************
    // ************************************************************************

    /**
     * Shows the frame with the memory.
     *
     * @param  flag  DOCUMENT ME!
     */
    public synchronized void setVisible(boolean flag) {
        setLocation(50, 50);
        super.setVisible(flag);
    }

    /**
     * Calls various methods based on the changes in the memory panel.
     *
     * @param  event  event that triggered function
     */
    public void stateChanged(ChangeEvent event) {
        Object source = event.getSource();
        MemoryMonitor mem = (MemoryMonitor) source;

        long inUse = mem.getUsed() / 1024;
        long tot = mem.getTotal() / 1024;

        used.setText(Long.toString(inUse));
        total.setText(Long.toString(tot));
        percentUsed.setText(Integer.toString(Math.round((float) (inUse) / tot * 100)));

        bm.setAmplitude(Math.round((float) (inUse) / tot * 100));
        bm.repaint();
        lm.setAmplitude((float) (inUse) / tot);
    }


    /**
     * Takes a txt field, and forces the textfield to accept numbers, backspace and delete-key entries.
     *
     * <p>also tells the pauseButton to click.</p>
     *
     * <p>This method should be checked periodically to see if there is a good way to ensure that the action within can
     * be superceded by the functionality in MipavUtil.makeNumericsOnly().</p>
     *
     * @param  txt  DOCUMENT ME!
     */
    protected void makeNumericsOnly(JTextField txt) {
        txt.addKeyListener(new KeyAdapter() { // make the field
                public void keyTyped(KeyEvent evt) { // not accept letters

                    //JTextField t = (JTextField) evt.getComponent();
                    char ch = evt.getKeyChar();

                    if (ch == KeyEvent.VK_ENTER) { // make sure the enter key acts as clicking OK

                        // Component c = t.getNextFocusableComponent();
                    } else if (((ch < '0') || (ch > '9')) &&
                                   ((ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE))) {

                        // if is the case that ch is outside the bounds of a number
                        // AND it is the case that ch is neither a BS or a DE,
                        // then...
                        // key is not a digit or a deletion char
                        evt.consume();
                    } else {
                        paused = false;
                        pauseButton.doClick();
                    }
                }
            });
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Tracks Memory allocated & used, and notifies anybody who is interested in finding out about it.
     */
    public class MemoryMonitor implements Runnable, FocusListener {

        /** DOCUMENT ME! */
        private ChangeEvent changeEvent;

        /** DOCUMENT ME! */
        private int counterGC;

        /** DOCUMENT ME! */
        private EventListenerList listenerList;

        /** DOCUMENT ME! */
        private int percentage;

        /** DOCUMENT ME! */
        private long sleepAmount = 1000;

        /** DOCUMENT ME! */
        private volatile Thread thread;

        /** DOCUMENT ME! */
        private float usedMemory, freeMemory, totalMemory;

        /**
         * Constructor. Creates the list for listeners
         */
        public MemoryMonitor() {
            listenerList = new EventListenerList();
        }


        // ****************** change listener support
        /**
         * add a memory change listener.
         *
         * @param  l  DOCUMENT ME!
         */
        public void addMemoryChangeListener(ChangeListener l) {
            (listenerList).add(ChangeListener.class, l);
        }


        // **************
        // FOCUS LISTENER
        /**
         * focus gained.
         *
         * @param  fe  DOCUMENT ME!
         */
        public void focusGained(FocusEvent fe) { }

        /**
         * when focus is lost the source is assumed to be a JTextField, and it sets the sample period.
         *
         * @param  fe  DOCUMENT ME!
         */
        public void focusLost(FocusEvent fe) {
            Object source = fe.getSource();

            if (!fe.isTemporary()) {

                if (source instanceof JTextField) {
                    sleepAmount = Long.parseLong(((JTextField) source).getText());
                }
            }
        }

        /**
         * The amount of free memory the thread found.
         *
         * @return  free memory in bytes
         */
        public long getFree() {
            return (long) (freeMemory);
        }

        /**
         * The percentage of used memory to total memory the thread found.
         *
         * @return  percent of used memory to total memeory in %
         */
        public int getPercentage() {
            return percentage;
        }

        /**
         * The amount of total memory the thread found.
         *
         * @return  total memory allocated by the JVM in bytes
         */
        public long getTotal() {
            return (long) totalMemory;
        }

        /**
         * The amount of used memory the thread found.
         *
         * @return  used memory in bytes
         */
        public long getUsed() {
            return (long) (usedMemory);
        }

        /**
         * removes the change listener.
         *
         * @param  l  DOCUMENT ME!
         */
        public void removeMemoryChangeListener(ChangeListener l) {
            (listenerList).remove(ChangeListener.class, l);
        }


        /**
         * when the thread wakes up, it collects information from the runtime on current memory status. It then notifies
         * all listeners.
         */
        public void run() {
            Thread me = Thread.currentThread();

            while (thread == me) {
                totalMemory = (float) MipavUtil.getMaxHeapMemory();
                freeMemory = (float) MipavUtil.getFreeHeapMemory();
                usedMemory = (float) totalMemory - freeMemory; // % used, that is
                percentage = (int) ((usedMemory / totalMemory * 100) + 0.5f);
                fireMemoryChanged(); // too many and this thread is selfish

                try {
                    Thread.sleep(sleepAmount);
                } catch (InterruptedException e) {
                    break;
                }

                // Because display process chews-up memory. Go call the garbage collector
                // every 90 calls to this process (if timer = 1000ms) then gc gets called
                // every 1.5 minutes.
                counterGC++;

                if (counterGC == 90) {
                    counterGC = 0;
                    System.gc();
                }
            }

            thread = null;
        }

        /**
         * Start the thread as a minimum priority thread.
         */
        public void start() {
            thread = new Thread(this);
            thread.setPriority(Thread.MIN_PRIORITY);
            thread.setName("MemoryMonitor");
            thread.start();
        }


        /**
         * kill the thread.
         */
        public void stop() {
            thread = null;
        }

        /**
         * Notify all listeners that have registered interest for notification on this event type. The event instance is
         * lazily created using the parameters passed into the fire method.
         */
        protected void fireMemoryChanged() {

            // Guaranteed to return a non-null array
            Object[] listeners = (listenerList).getListenerList();

            // Process the listeners last to first, notifying
            // those that are interested in this event
            for (int i = listeners.length - 2; i >= 0; i -= 2) {

                if (listeners[i] == ChangeListener.class) {

                    // Lazily create the event:
                    if (changeEvent == null) {
                        changeEvent = new ChangeEvent(this);
                    }

                    ((ChangeListener) listeners[i + 1]).stateChanged(changeEvent);

                }
            }
        }

    }

}
