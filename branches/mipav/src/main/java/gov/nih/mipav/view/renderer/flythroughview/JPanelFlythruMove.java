package gov.nih.mipav.view.renderer.flythroughview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;

import java.awt.*;
import java.awt.event.*;

import java.io.File;
import java.util.*;

import javax.media.MediaLocator;
import javax.swing.*;
import javax.swing.border.*;


/**
 * <p>Title: JPanelFlythruMove</p>
 *
 * <p>Description: Fly thru user control panel that manipulate the movements.</p>
 *
 * @author  Ruida Cheng
 */
public class JPanelFlythruMove extends JPanelRendererBase implements ActionListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6224214629454958231L;

    /** Recorder/Player is stopped. */
    public static final int STOP_MODE = 0;

    /** Recorder/Player is recording. */
    public static final int RECORD_MODE = 1;

    /** Recorder/Player is paused. */
    public static final int PAUSE_MODE = 2;

    /** Recorder/Player is playing. */
    public static final int PLAY_MODE = 3;

    /** AVI is recording. */
    public static final int AVI_MODE = 4;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Flag to indicate first event. */
    public boolean first = true;

    /** Current mode, init to stop. */
    public int mode = STOP_MODE;

    /** flag to indicate current mode being changed. */
    boolean changedMode = false;

    /** index to the item list. */
    int current = 0;

    /** Current mouse press event time stamp. */
    long currEventTime;

    /** Flag to indicating play mouse is running or not. */
    boolean isPlaying = false;

    /** Mouse event counter. */
    int mouseCount = 1;

    /** Previous mouse press event time stamp. */
    long prevEventTime;

    /** DOCUMENT ME! */
    private JButton annotationNextButton;

    /** DOCUMENT ME! */
    private JButton annotationPrevButton;

    /** Auto Run Button. */
    private JButton autoRunButton;

    /** AVI Recorder Play button. */
    private JToggleButton aviPlayButton;

    /** AVI Recorder record button. */
    private JToggleButton aviRecordButton;

    /** AVI Recorder Stop button. */
    private JToggleButton aviStopButton;

    /** DOCUMENT ME! */
    private JButton branchButton;

    /** Continue play button. */
    private JToggleButton contButton;

    /** DOCUMENT ME! */
    private JButton endButton;

    /** Seqence of mouse events ( press, shift,and release as individual mouse event). */
    private MouseEventVector eventVector;

    /** DOCUMENT ME! */
    private JButton gazeDecreaseButton;

    /** DOCUMENT ME! */
    private JButton gazeIncrButton;

    /** DOCUMENT ME! */
    private JButton homeButton;

    /** DOCUMENT ME! */
    private JButton leftRotateButton;

    /** MS media player button to play AVI file. */
    private JButton mediaPlayerButton;

    /** DOCUMENT ME! */
    private JButton middleDownButton;

    /** DOCUMENT ME! */
    private JButton middleLeftButton;

    /** Empty button group. */
    private JButton middleLeftButtonEmpty;

    /** DOCUMENT ME! */
    private JButton middleRightButton;

    /** DOCUMENT ME! */
    private JButton middleUpButton;

    /** Sequence of mouse events( press, shift and release as a whole mouse event). */
    private Vector mouseEvents;

    /** Panel holds the mouse move buttons. */
    private JPanel mousePanel;

    /** Movie control panel that hold mouse recorder and AVI movie control. */
    private JPanel moviePanel;

    /** The parent fly thru render reference. */
    private FlyThroughRenderInterface parentScene;

    /** Pause button. */
    private JToggleButton pauseButton;

    /** Display button. */
    private JToggleButton playButton;

    /** Reference to PlayMouse class object. */
    private PlayMouse playMouse;

    /** If any of the mouse move button pressed. */
    private boolean pressed;

    /** Apple quick time player button to play quick time movie. */
    private JButton quickTimeButton;

    /** Recorder button. */
    private JToggleButton recordButton;

    /** DOCUMENT ME! */
    private JButton reverseButton;

    /** DOCUMENT ME! */
    private JButton rightDownButton;

    /** DOCUMENT ME! */
    private JButton rightLeftButton;

    /** DOCUMENT ME! */
    private JButton rightRightButton;

    /** DOCUMENT ME! */
    private JButton rightRotateButton;

    /** JButton group. */
    private JButton rightUpButton;

    /** Scroll pane. */
    private JScrollPane scroller;

    /** Scroll panel that holding the all the control components. */
    private DrawingPanel scrollPanel;

    /** Next step button. */
    private JToggleButton stepButton;

    /** DOCUMENT ME! */
    private JButton stepDecreaseButton;

    /** DOCUMENT ME! */
    private JButton stepIncrButton;

    /** Stop button. */
    private JToggleButton stopButton;

    /** Time to wait for the next mouse event. */
    private long time = 10;

    /** Tool bar of the mouse recorder. */
    private JToolBar toolBar;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * User interface to control the fly through renderer events.
     *
     * @param  _kView  FlythruRender
     */
    public JPanelFlythruMove(FlyThroughRenderInterface _kView) {
        addKeyListener(this);
        parentScene = _kView;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Invoke the action event accroding to the comamnd passed in.
     *
     * @param  event  ActionEvent
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("QuickTime")) {
            saveQuickTimeMovie();
        } else if (command.equals("AVI")) {
            saveAVIMovie();
        } else if (command.equals("Home")) {
            parentScene.makeMove("home");
        } else if (command.equals("End")) {
            parentScene.makeMove("end");
        } else if (command.equals("Reverse")) {
            parentScene.makeMove("reverse");
        } else if (command.equals("Branch")) {
            parentScene.makeMove("nextBranch");
        } else if (command.equals("stepDistanceIncrease")) {
            parentScene.makeMove("stepDistanceIncrease");
        } else if (command.equals("stepDistanceDecrease")) {
            parentScene.makeMove("stepDistanceDecrease");
        } else if (command.equals("gazeDistanceIncrease")) {
            parentScene.makeMove("gazeDistanceIncrease");
        } else if (command.equals("gazeDistanceDecrease")) {
            parentScene.makeMove("gazeDistanceDecrease");
        } else if (command.equals("AnnotatePrev")) {
            parentScene.makeMove("prevAnnotatePt");
        } else if (command.equals("AnnotateNext")) {
            parentScene.makeMove("nextAnnotatePt");
        } else if (command.equals("AVIrecord")) {

            if (!isAVIRecording()) {
                changedMode = true;
                first = true;
                parentScene.record(true);
            }

            mode = AVI_MODE;
        } else if (command.equals("AVIstop")) {

            if (isAVIRecording()) {
                mode = STOP_MODE;
                parentScene.record(false);
            }
        } else if (command.equals("AVIplay")) {
            mode = PLAY_MODE;
            new ViewJFrameAnimateClip(parentScene.getImage(), parentScene.getWidth(), parentScene.getHeight(), parentScene.getCounter());
        } else if (command.equals("Record")) {

            if (!isRecording()) {
                changedMode = true;
                first = true;
            }

            mode = RECORD_MODE;

            if (mouseEvents == null) {
                mouseEvents = new Vector();
            } else {
                mouseEvents.removeAllElements();
            }

        } else if (command.equals("Play") && !isPlaying()) {
            changedMode = true;
            mode = PLAY_MODE;

            // disable record while playing, so we don't record the
            // events we're playing
            recordButton.setEnabled(false);
            contButton.setEnabled(false);
            stepButton.setEnabled(false);

            if ((mouseEvents != null) && !mouseEvents.isEmpty()) {
                play(true, false);
            }
        } else if (command.equals("ContPlay") && !isPlaying()) {
            changedMode = true;
            mode = PLAY_MODE;

            // disable record while playing, so we don't record the
            // events we're playing
            recordButton.setEnabled(false);
            aviRecordButton.setEnabled(false);
            playButton.setEnabled(false);
            stepButton.setEnabled(false);

            if ((mouseEvents != null) && !mouseEvents.isEmpty()) {
                play(true, true);
            }

        } else if (command.equals("Step") && !isPlaying()) {
            changedMode = true;
            mode = PLAY_MODE;

            // disable record while playing, so we don't record the
            // events we're playing
            recordButton.setEnabled(false);
            aviRecordButton.setEnabled(false);
            contButton.setEnabled(false);
            playButton.setEnabled(false);

            if ((mouseEvents != null) && !mouseEvents.isEmpty()) {
                play(false, false);
            }

        } else if (command.equals("Pause")) {

            if (!isPaused()) {
                changedMode = true;
            }

            mode = PAUSE_MODE;

            if ((playMouse != null) && playMouse.isAlive()) {
                playMouse.interrupt();
            }
        } else if (command.equals("Stop")) {

            if (!isStopped()) {
                changedMode = true;
            }

            mode = STOP_MODE;

            if ((playMouse != null) && playMouse.isAlive()) {
                playMouse.interrupt();
            }

            playButton.setEnabled(true);
            contButton.setEnabled(true);
            stepButton.setEnabled(true);
            pauseButton.setEnabled(true);
            recordButton.setEnabled(true);
            aviRecordButton.setEnabled(true);
        } else if (command.equals("AutoRun")) {
            parentScene.autoRun();
        }

    }

    /**
     * Dispose memory.
     *
     * @param  flag  invoke the super dispose or not.
     */
    public void dispose(boolean flag) {
        parentScene = null;
        scrollPanel = null;
        scroller = null;
        mousePanel = null;

        rightUpButton = null;
        rightDownButton = null;
        rightLeftButton = null;
        rightRightButton = null;
        middleUpButton = null;
        middleDownButton = null;
        middleLeftButton = null;
        middleRightButton = null;

        homeButton = null;
        endButton = null;
        reverseButton = null;
        branchButton = null;
        stepIncrButton = null;
        stepDecreaseButton = null;
        gazeIncrButton = null;
        gazeDecreaseButton = null;
        rightRotateButton = null;
        leftRotateButton = null;
        annotationPrevButton = null;
        annotationNextButton = null;

        middleLeftButtonEmpty = null;

        moviePanel = null;
        aviRecordButton = null;
        aviPlayButton = null;
        aviStopButton = null;
        mediaPlayerButton = null;
        quickTimeButton = null;
        recordButton = null;
        playButton = null;
        stepButton = null;
        contButton = null;
        pauseButton = null;
        stopButton = null;
        autoRunButton = null;
        toolBar = null;
        playMouse = null;

        mouseEvents = null;
        eventVector = null;

        if (flag == true) {
            super.disposeLocal();
        }

    }

    /**
     * Return the main control panel.
     *
     * @return  JPanel the main control panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Returns whether the AVI is recording.
     *
     * @return  whether the panel is recording
     */
    public boolean isAVIRecording() {
        return mode == AVI_MODE;
    }

    /**
     * Returns whether the mouse recorder is paused.
     *
     * @return  whether the mouse recorder is paused
     */
    public boolean isPaused() {
        return mode == PAUSE_MODE;
    }

    /**
     * Returns whether the mouse recorder is playing saved events.
     *
     * @return  whether the panel is playing a set of saved events
     */
    public boolean isPlaying() {
        return mode == PLAY_MODE;
    }

    /**
     * Returns whether the mouse recorder is recording.
     *
     * @return  whether the panel is recording
     */
    public boolean isRecording() {
        return mode == RECORD_MODE;
    }

    /**
     * Returns whether the mouse recorder is stopped.
     *
     * @return  whether the mouse recorder is stopped
     */
    public boolean isStopped() {
        return mode == STOP_MODE;
    }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mouseClicked(MouseEvent event) { }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mouseEntered(MouseEvent event) { }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mouseExited(MouseEvent event) { }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */

    public void mousePressed(MouseEvent event) {
        setIcon(event.getSource(), true);

        currEventTime = event.getWhen();

        // System.out.println("time elapse = " + (currentTime-prevTime));
        if ((currEventTime - prevEventTime) < 1000) {
            pressed = false;

            return;
        }

        pressed = true;

        if (isRecording()) {
            eventVector = new MouseEventVector("", null, first, parentScene.getBranchState(), 0);
            eventVector.add(event, parentScene.getBranchState());
            // currentPathDist = parentScene.getPathDist();
        }

        StandardMouse mouse = new StandardMouse(event);

        prevEventTime = event.getWhen();
        mouse.start();
    }

    /**
     * Translates the event to the appropriate version on the canvas, then sends it to the canvas.
     *
     * @param  event  Original mouse event.
     */
    public void mouseReleased(MouseEvent event) {
        setIcon(event.getSource(), false);
        pressed = false;

        if (isRecording()) {
            eventVector.add(event, parentScene.getBranchState());
            mouseEvents.add(eventVector);
        }
    }

    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   DOCUMENT ME!
     * @param  frameHeight  DOCUMENT ME!
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - (toolBar.getHeight() * 2)));
        scroller.setSize(new Dimension(panelWidth, frameHeight - (toolBar.getHeight() * 2)));
        scroller.revalidate();
    }

    /**
     * Creates the mouse control panels. There are four arrows for each mouse button, left, right, and middle.
     *
     * @return  The panel containing the mouse controls.
     */
    private JPanel buildControlPanel() {

        JToolBar toolBar = null;
        TitledBorder title = null;

        title = buildTitledBorder("Move control");
        toolBar = new JToolBar();
        toolBar.setBorder(title);

        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        toolBar.setFloatable(false);

        middleUpButton = new UpButton("Middle mouse button up");
        middleDownButton = new DownButton("Middle mouse button down");
        middleRightButton = new RightButton("Middle mouse button right");
        middleLeftButton = new LeftButton("Middle mouse button left");
        middleLeftButtonEmpty = new EmptyButton("");

        homeButton = new JButton();
        homeButton.setIcon(MipavUtil.getIcon("home.gif"));
        homeButton.setRolloverIcon(MipavUtil.getIcon("homeroll.gif"));
        homeButton.addActionListener(this);
        homeButton.setToolTipText("Move to starting point");
        homeButton.setActionCommand("Home");
        homeButton.setBorderPainted(false);
        homeButton.setEnabled(true);
        toolBar.add(homeButton);

        endButton = new JButton();
        endButton.setIcon(MipavUtil.getIcon("end.gif"));
        endButton.setRolloverIcon(MipavUtil.getIcon("endroll.gif"));
        endButton.addActionListener(this);
        endButton.setToolTipText("Move to ending point");
        endButton.setActionCommand("End");
        endButton.setBorderPainted(false);
        endButton.setEnabled(true);
        toolBar.add(endButton);

        reverseButton = new JButton();
        reverseButton.setIcon(MipavUtil.getIcon("refresh.gif"));
        reverseButton.setRolloverIcon(MipavUtil.getIcon("refreshroll.gif"));
        reverseButton.addActionListener(this);
        reverseButton.setToolTipText("Reverse the tracking path");
        reverseButton.setActionCommand("Reverse");
        reverseButton.setBorderPainted(false);
        reverseButton.setEnabled(true);
        toolBar.add(reverseButton);

        branchButton = new JButton();
        branchButton.setIcon(MipavUtil.getIcon("branch.gif"));
        branchButton.setRolloverIcon(MipavUtil.getIcon("branchroll.gif"));
        branchButton.addActionListener(this);
        branchButton.setToolTipText("Branch to the next tracking path");
        branchButton.setActionCommand("Branch");
        branchButton.setBorderPainted(false);
        branchButton.setEnabled(true);
        toolBar.add(branchButton);

        rightRotateButton = new JButton();
        rightRotateButton.setIcon(MipavUtil.getIcon("clockwiserotate.gif"));
        rightRotateButton.setRolloverIcon(MipavUtil.getIcon("clockwiserotateroll.gif"));
        rightRotateButton.addActionListener(this);
        rightRotateButton.setToolTipText("Clockwise rotation");
        rightRotateButton.setActionCommand("Clockwise");
        rightRotateButton.setBorderPainted(false);
        rightRotateButton.setEnabled(true);
        rightRotateButton.addMouseListener(this);
        toolBar.add(rightRotateButton);

        leftRotateButton = new JButton();
        leftRotateButton.setIcon(MipavUtil.getIcon("cntclockrotate.gif"));
        leftRotateButton.setRolloverIcon(MipavUtil.getIcon("cntclockrotateroll.gif"));
        leftRotateButton.addActionListener(this);
        leftRotateButton.setToolTipText("Counter clockwise rotation");
        leftRotateButton.setActionCommand("CounterClockwise");
        leftRotateButton.setBorderPainted(false);
        leftRotateButton.setEnabled(true);
        leftRotateButton.addMouseListener(this);
        toolBar.add(leftRotateButton);

        annotationPrevButton = new JButton();
        annotationPrevButton.setIcon(MipavUtil.getIcon("prevannotateptr.gif"));
        annotationPrevButton.setRolloverIcon(MipavUtil.getIcon("prevannotateptrroll.gif"));
        annotationPrevButton.addActionListener(this);
        annotationPrevButton.setToolTipText("Previous annotation point");
        annotationPrevButton.setActionCommand("AnnotatePrev");
        annotationPrevButton.setBorderPainted(false);
        annotationPrevButton.setEnabled(true);
        toolBar.add(annotationPrevButton);

        annotationNextButton = new JButton();
        annotationNextButton.setIcon(MipavUtil.getIcon("nextannotateptr.gif"));
        annotationNextButton.setRolloverIcon(MipavUtil.getIcon("nextannotateptrroll.gif"));
        annotationNextButton.addActionListener(this);
        annotationNextButton.setToolTipText("Next annotation point");
        annotationNextButton.setActionCommand("AnnotateNext");
        annotationNextButton.setBorderPainted(false);
        annotationNextButton.setEnabled(true);
        toolBar.add(annotationNextButton);

        stepDecreaseButton = new JButton();
        stepDecreaseButton.setIcon(MipavUtil.getIcon("stepdecrease.gif"));
        stepDecreaseButton.setRolloverIcon(MipavUtil.getIcon("stepdecreaseroll.gif"));
        stepDecreaseButton.addActionListener(this);
        stepDecreaseButton.setToolTipText("Decrease the step distance");
        stepDecreaseButton.setActionCommand("stepDistanceDecrease");
        stepDecreaseButton.setBorderPainted(false);
        stepDecreaseButton.setEnabled(true);
        toolBar.add(stepDecreaseButton);

        stepIncrButton = new JButton();
        stepIncrButton.setIcon(MipavUtil.getIcon("stepincrease.gif"));
        stepIncrButton.setRolloverIcon(MipavUtil.getIcon("stepincreaseroll.gif"));
        stepIncrButton.addActionListener(this);
        stepIncrButton.setToolTipText("Increase the step distance");
        stepIncrButton.setActionCommand("stepDistanceIncrease");
        stepIncrButton.setBorderPainted(false);
        stepIncrButton.setEnabled(true);
        toolBar.add(stepIncrButton);

        gazeDecreaseButton = new JButton();
        gazeDecreaseButton.setIcon(MipavUtil.getIcon("gazedecrease.gif"));
        gazeDecreaseButton.setRolloverIcon(MipavUtil.getIcon("gazedecreaseroll.gif"));
        gazeDecreaseButton.addActionListener(this);
        gazeDecreaseButton.setToolTipText("Decrease the watching aim distance");
        gazeDecreaseButton.setActionCommand("gazeDistanceDecrease");
        gazeDecreaseButton.setBorderPainted(false);
        gazeDecreaseButton.setEnabled(true);
        toolBar.add(gazeDecreaseButton);

        gazeIncrButton = new JButton();
        gazeIncrButton.setIcon(MipavUtil.getIcon("gazeincrease.gif"));
        gazeIncrButton.setRolloverIcon(MipavUtil.getIcon("gazeincreaseroll.gif"));
        gazeIncrButton.addActionListener(this);
        gazeIncrButton.setToolTipText("Increase the watching aim distance");
        gazeIncrButton.setActionCommand("gazeDistanceIncrease");
        gazeIncrButton.setBorderPainted(false);
        gazeIncrButton.setEnabled(true);
        toolBar.add(gazeIncrButton);

        JButton middleRightButtonEmpty = new EmptyButton("");

        middleLeftButtonEmpty = new EmptyButton("");

        JPanel controlButtonPanel = new JPanel();

        controlButtonPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        controlButtonPanel.add(toolBar);

        JPanel leftMousePanel = new JPanel();

        leftMousePanel.setLayout(new GridBagLayout());
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        leftMousePanel.add(middleUpButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        leftMousePanel.add(middleLeftButtonEmpty, gbc);
        middleLeftButtonEmpty.setEnabled(false);
        gbc.gridx = 2;
        leftMousePanel.add(middleRightButtonEmpty, gbc);
        middleRightButtonEmpty.setEnabled(false);
        gbc.gridx = 1;
        gbc.gridy = 2;
        leftMousePanel.add(middleDownButton, gbc);
        leftMousePanel.setBorder(buildTitledBorder("Forward/Backward"));

        rightUpButton = new UpButton("Right mouse button up");
        rightDownButton = new DownButton("Right mouse button down");
        rightRightButton = new RightButton("Right mouse button right");
        rightLeftButton = new LeftButton("Right mouse button left");

        JPanel rightMousePanel = new JPanel();

        rightMousePanel.setLayout(new GridBagLayout());
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        rightMousePanel.add(rightUpButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        rightMousePanel.add(rightLeftButton, gbc);
        gbc.gridx = 2;
        rightMousePanel.add(rightRightButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        rightMousePanel.add(rightDownButton, gbc);
        rightMousePanel.setBorder(buildTitledBorder("View"));

        mousePanel = new JPanel();
        mousePanel.setLayout(new BoxLayout(mousePanel, BoxLayout.Y_AXIS));

        JPanel addAnnotatePtPanel = new JPanel();
        JLabel annotateLabel = new JLabel("Shift + left mouse click to add annotation point");

        addAnnotatePtPanel.add(annotateLabel);

        JPanel movePanel = new JPanel();

        movePanel.setLayout(new BoxLayout(movePanel, BoxLayout.X_AXIS));
        movePanel.add(leftMousePanel);
        movePanel.add(rightMousePanel);

        mousePanel.add(controlButtonPanel);
        mousePanel.add(addAnnotatePtPanel);
        mousePanel.add(movePanel);

        return mousePanel;
    }

    /**
     * Build the mouse recorder button panel.
     */
    private void buildMoviePanel() {
        JToolBar toolBar = null;
        ButtonGroup group = null;
        ButtonGroup aviGroup = null;
        TitledBorder title = null;
        Insets insets = null;

        try {
            toolBar = new JToolBar();
            group = new ButtonGroup();
            aviGroup = new ButtonGroup();
            title = buildTitledBorder("Mouse recorder");
            aviRecordButton = new JToggleButton(MipavUtil.getIcon("avirecord.gif"), false);
            aviPlayButton = new JToggleButton(MipavUtil.getIcon("aviplay.gif"), false);
            aviStopButton = new JToggleButton(MipavUtil.getIcon("avistop.gif"), false);
            recordButton = new JToggleButton(MipavUtil.getIcon("record.gif"), false);
            playButton = new JToggleButton(MipavUtil.getIcon("play.gif"), false);
            contButton = new JToggleButton(MipavUtil.getIcon("rightcont.gif"), false);
            stepButton = new JToggleButton(MipavUtil.getIcon("step.gif"), false);
            pauseButton = new JToggleButton(MipavUtil.getIcon("pause.gif"), false);
            stopButton = new JToggleButton(MipavUtil.getIcon("stop.gif"), true);
            autoRunButton = new JButton(MipavUtil.getIcon("movie.gif"));
            mediaPlayerButton = new JButton(MipavUtil.getIcon("avisave.gif"));
            quickTimeButton = new JButton(MipavUtil.getIcon("movsave.gif"));
            insets = new Insets(0, 0, 0, 0);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in mouse recorder.");
        }

        toolBar.setBorder(title);
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        toolBar.setFloatable(false);

        recordButton.addActionListener(this);
        recordButton.setActionCommand("Record");
        recordButton.setMargin(insets);
        recordButton.setToolTipText("Record mouse actions");
        recordButton.setBorderPainted(false);
        recordButton.setFocusPainted(false);
        recordButton.setSelectedIcon(MipavUtil.getIcon("recordpress.gif"));
        recordButton.setContentAreaFilled(false);
        toolBar.add(recordButton);
        group.add(recordButton);

        playButton.addActionListener(this);
        playButton.setActionCommand("Play");
        playButton.setMargin(insets);
        playButton.setToolTipText("Play mouse actions");
        playButton.setBorderPainted(false);
        playButton.setFocusPainted(false);
        playButton.setSelectedIcon(MipavUtil.getIcon("playpress.gif"));
        playButton.setContentAreaFilled(false);
        toolBar.add(playButton);
        group.add(playButton);

        contButton.addActionListener(this);
        contButton.setActionCommand("ContPlay");
        contButton.setMargin(insets);
        contButton.setToolTipText("Continuously play mouse actions");
        contButton.setBorderPainted(false);
        contButton.setFocusPainted(false);
        contButton.setSelectedIcon(MipavUtil.getIcon("rightcontpress.gif"));
        contButton.setContentAreaFilled(false);
        toolBar.add(contButton);
        group.add(contButton);

        stepButton.addActionListener(this);
        stepButton.setActionCommand("Step");
        stepButton.setMargin(insets);
        stepButton.setToolTipText("Play one mouse action");
        stepButton.setBorderPainted(false);
        stepButton.setFocusPainted(false);
        stepButton.setSelectedIcon(MipavUtil.getIcon("steppress.gif"));
        stepButton.setContentAreaFilled(false);
        toolBar.add(stepButton);
        group.add(stepButton);

        pauseButton.addActionListener(this);
        pauseButton.setMargin(insets);
        pauseButton.setToolTipText("Pause");
        pauseButton.setActionCommand("Pause");
        pauseButton.setBorderPainted(false);
        pauseButton.setFocusPainted(false);
        pauseButton.setSelectedIcon(MipavUtil.getIcon("pausepress.gif"));
        pauseButton.setContentAreaFilled(false);
        toolBar.add(pauseButton);
        group.add(pauseButton);

        stopButton.addActionListener(this);
        stopButton.setMargin(insets);
        stopButton.setToolTipText("Stop");
        stopButton.setActionCommand("Stop");
        stopButton.setBorderPainted(false);
        stopButton.setFocusPainted(false);
        stopButton.setSelectedIcon(MipavUtil.getIcon("stoppress.gif"));
        stopButton.setContentAreaFilled(false);
        toolBar.add(stopButton);
        group.add(stopButton);

        toolBar.add(makeSeparator());

        aviRecordButton.addActionListener(this);
        aviRecordButton.setActionCommand("AVIrecord");
        aviRecordButton.setMargin(insets);
        aviRecordButton.setToolTipText("Record AVI movie");
        aviRecordButton.setBorderPainted(false);
        aviRecordButton.setFocusPainted(false);
        aviRecordButton.setSelectedIcon(MipavUtil.getIcon("avirecordroll.gif"));
        aviRecordButton.setContentAreaFilled(false);
        toolBar.add(aviRecordButton);
        aviGroup.add(aviRecordButton);

        aviPlayButton.addActionListener(this);
        aviPlayButton.setActionCommand("AVIplay");
        aviPlayButton.setMargin(insets);
        aviPlayButton.setToolTipText("Play AVI movie");
        aviPlayButton.setBorderPainted(false);
        aviPlayButton.setFocusPainted(false);
        aviPlayButton.setSelectedIcon(MipavUtil.getIcon("aviplayroll.gif"));
        aviPlayButton.setContentAreaFilled(false);
        toolBar.add(aviPlayButton);
        aviGroup.add(aviPlayButton);

        aviStopButton.addActionListener(this);
        aviStopButton.setActionCommand("AVIstop");
        aviStopButton.setMargin(insets);
        aviStopButton.setToolTipText("Stop AVI movie");
        aviStopButton.setBorderPainted(false);
        aviStopButton.setFocusPainted(false);
        aviStopButton.setSelectedIcon(MipavUtil.getIcon("avistoproll.gif"));
        aviStopButton.setContentAreaFilled(false);
        toolBar.add(aviStopButton);
        aviGroup.add(aviStopButton);

        mediaPlayerButton.addActionListener(this);
        mediaPlayerButton.setMargin(insets);
        mediaPlayerButton.setToolTipText("Save AVI movie");
        mediaPlayerButton.setActionCommand("AVI");
        mediaPlayerButton.setBorderPainted(false);
        mediaPlayerButton.setRolloverEnabled(true);
        mediaPlayerButton.setRolloverIcon(MipavUtil.getIcon("avisaveroll.gif"));
        mediaPlayerButton.setFocusPainted(false);
        toolBar.add(mediaPlayerButton);

        quickTimeButton.addActionListener(this);
        quickTimeButton.setMargin(insets);
        quickTimeButton.setToolTipText("Save QuickTime movie");
        quickTimeButton.setActionCommand("QuickTime");
        quickTimeButton.setBorderPainted(false);
        quickTimeButton.setRolloverEnabled(true);
        quickTimeButton.setRolloverIcon(MipavUtil.getIcon("movsaveroll.gif"));
        quickTimeButton.setFocusPainted(false);
        toolBar.add(quickTimeButton);

        toolBar.add(makeSeparator());

        autoRunButton.addActionListener(this);
        autoRunButton.setMargin(insets);
        autoRunButton.setToolTipText("Round trip auto run");
        autoRunButton.setActionCommand("AutoRun");
        autoRunButton.setContentAreaFilled(false);
        toolBar.add(autoRunButton);

        toolBar.setFloatable(false);

        moviePanel = new JPanel();
        moviePanel.setLayout(new BoxLayout(moviePanel, BoxLayout.X_AXIS));
        moviePanel.add(toolBar);

    }

    /**
     * Initialize the control panel.
     */
    private void init() {

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());

        // Put the drawing area in a scroll pane.
        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel(new GridBagLayout());

        buildControlPanel();
        buildMoviePanel();

        Box contentBox = new Box(BoxLayout.Y_AXIS);

        contentBox.add(moviePanel);
        contentBox.add(mousePanel);

        scrollPanel.add(contentBox, BorderLayout.NORTH);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(scroller, gbc);
    }

    /**
     * Makes a separator for the use in the toolbars.
     *
     * @return  Separator button.
     */
    private JButton makeSeparator() {
        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));

        separator.setBorderPainted(false);
        separator.setFocusPainted(false);

        return (separator);
    }

    /**
     * If there is no other thread running, starts a new thread to play the mouse events.
     *
     * @param  selectedToEnd  If <code>true</code>, the mouse events are played from the selected one to the end of the
     *                        list. If <code>false</code> only the selected one is played.
     * @param  forever        If <code>true</code>, the mouse events list is played until the stop button or pause
     *                        button is pressed.
     */

    private void play(boolean selectedToEnd, boolean forever) {

        if (!((playMouse != null) && playMouse.isAlive())) {

            try {
                playMouse = new PlayMouse(selectedToEnd, forever);
                playMouse.start();
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in mouse recorder.");

                return;
            }
        }
    }

    /**
     * Sets the button properties for all the arrow buttons.
     *
     * @param  button  Button whose properties need to be set.
     * @param  tip     Tool tip text to associate with this button.
     */
    private void setButtonProps(JButton button, String tip) {
        button.setRolloverEnabled(true);
        button.setMargin(new Insets(0, 0, 0, 0));
        button.setToolTipText(tip);
        button.setBorderPainted(false);
        button.setFocusPainted(false);
        button.setContentAreaFilled(false);
        button.addMouseListener(this);
    }

    /**
     * Sets the pressed or unpressed icon for the button, depending on which type it is.
     *
     * @param  source  The button that was pressed or released.
     * @param  press   <code>true</code> indicates the button was pressed, <code>false</code> that it was released.
     */
    private void setIcon(Object source, boolean press) {

        if (press) {

            if (source instanceof UpButton) {
                ((UpButton) source).setIcon(MipavUtil.getIcon("uppress.gif"));
            } else if (source instanceof DownButton) {
                ((DownButton) source).setIcon(MipavUtil.getIcon("downpress.gif"));
            } else if (source instanceof RightButton) {
                ((RightButton) source).setIcon(MipavUtil.getIcon("rightarrowpress.gif"));
            } else if (source instanceof LeftButton) {
                ((LeftButton) source).setIcon(MipavUtil.getIcon("leftarrowpress.gif"));
            }

            ((JButton) source).setRolloverEnabled(false);
        } else {
            ((JButton) source).setRolloverEnabled(true);

            if (source instanceof UpButton) {
                ((UpButton) source).setIcon(MipavUtil.getIcon("up.gif"));
            } else if (source instanceof DownButton) {
                ((DownButton) source).setIcon(MipavUtil.getIcon("down.gif"));
            } else if (source instanceof RightButton) {
                ((RightButton) source).setIcon(MipavUtil.getIcon("rightarrow.gif"));
            } else if (source instanceof LeftButton) {
                ((LeftButton) source).setIcon(MipavUtil.getIcon("leftarrow.gif"));
            }
        }
    }
    

    /**
     * Save quick time movie.
     */
    public void saveAVIMovie() {
        File outputFile = null;
        File[] inputFile = new File[parentScene.getCounter()];
        String directory = parentScene.getImage().getFileInfo(0).getFileDirectory();
        for (int i = 0; i < parentScene.getCounter(); i++) {
            inputFile[i] = new File(directory + "captureImage" + i + "." + "jpg");
        }

        // Save to AVI file.
        String file = directory + "flythru.avi";
        outputFile = new File(file);

        try {
            MovieMaker movieMake = new MovieMaker(parentScene.getWidth(), parentScene.getHeight(), 3, outputFile, inputFile);
            movieMake.makeMovie();
        } catch (Throwable t) {
            t.printStackTrace();
        }

        inputFile = null;
        outputFile = null;

    }

    /**
     * Save AVI movie.
     */
    public void saveQuickTimeMovie() {
        MediaLocator oml;
        Vector inputFiles = new Vector();
        String file;

        String directory = parentScene.getImage().getFileInfo(0).getFileDirectory() + "flythru" + File.separatorChar;
        file = "file:" + directory + "flythru.mov";

        if ((oml = new MediaLocator(file)) == null) {
            System.err.println("Cannot build media locator from: " + directory);

            return;
        }

        for (int i = 0; i < parentScene.getCounter(); i++) {
            inputFiles.addElement(directory + "captureImage" + i + "." + "jpg");
        }

        JpegImagesToMovie imageToMovie = new JpegImagesToMovie();
        imageToMovie.doIt(parentScene.getWidth(), parentScene.getHeight(), 3, inputFiles, oml);

        inputFiles = null;
        oml = null;

    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Helper class which sets the appropriate icons for the down button. Will use "instanceof" keyword to determine
     * icons later.
     */
    class DownButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -7732406232133972238L;

        /**
         * Creates new button with correct icon and given tool tip.
         *
         * @param  tip  Tool tip for button.
         */
        public DownButton(String tip) {
            super(MipavUtil.getIcon("down.gif"));
            setRolloverIcon(MipavUtil.getIcon("downroll.gif"));
            setButtonProps(this, tip);
        }
    }


    /**
     * Wrapper in order to hold the control panel layout in the JScrollPane.
     */
    class DrawingPanel extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 2272073315047909207L;

        /**
         * DOCUMENT ME!
         *
         * @param  g  DOCUMENT ME!
         */
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);

        }
    }


    /**
     * DOCUMENT ME!
     */
    class EmptyButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 4855906570406134228L;

        /**
         * Creates new button with correct icon and given tool tip.
         *
         * @param  tip  Tool tip for button.
         */
        public EmptyButton(String tip) {
            super(MipavUtil.getIcon("emptycursor.gif"));
            setRolloverIcon(MipavUtil.getIcon("emptycursor.gif"));
            setButtonProps(this, tip);
        }
    }


    /**
     * Helper class which sets the appropriate icons for the left button. Will use "instanceof" keyword to determine
     * icons later.
     */
    class LeftButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -2440368436767136480L;

        /**
         * Creates new button with correct icon and given tool tip.
         *
         * @param  tip  Tool tip for button.
         */
        public LeftButton(String tip) {
            super(MipavUtil.getIcon("leftarrow.gif"));
            setRolloverIcon(MipavUtil.getIcon("leftarrowroll.gif"));
            setButtonProps(this, tip);
        }
    }


    /**
     * Helper class which sets the appropriate icons for the left button. Will use "instanceof" keyword to determine
     * icons later.
     */
    class LeftRotateButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 1669360199493100208L;

        /**
         * Creates new button with correct icon and given tool tip.
         *
         * @param  tip  Tool tip for button.
         */
        public LeftRotateButton(String tip) {
            super(MipavUtil.getIcon("leftrotate.gif"));
            setRolloverIcon(MipavUtil.getIcon("leftrotateroll.gif"));
            setButtonProps(this, tip);
        }
    }

    /**
     * Thread that plays. Must be a thread so that the stop and pause buttons work. Plays either one mouse event at a
     * time, the whole list from the selected index, or the whole list continuously.
     */
    class PlayMouse extends Thread {

        /** DOCUMENT ME! */
        boolean forever;

        /** DOCUMENT ME! */
        boolean selectedToEnd;

        /**
         * Constructs a new thread and initializes the parameters which dictate what kind of play this is.*
         *
         * @param  selectedToEnd  <code>true</code> indicates play the list of mouse events in sequential order from the
         *                        first selected index. <code>false</code> indicates only play the selected mouse event.
         *                        *
         * @param  forever        <code>true</code> indicates keep playing mouse events until the stop or pause button
         *                        is pressed.
         */
        public PlayMouse(boolean selectedToEnd, boolean forever) {
            this.selectedToEnd = selectedToEnd;
            this.forever = forever;
            isPlaying = true;
        }

        /**
         * Runs the thread and plays according to the established parameters.
         */
        public void run() {
            int stop = 0;
            MouseEventVector vector;
            long when = 0;
            long newWhen = 0;
            Object source;

            // if full list, play until end of list; otherwise just play one event.
            if (selectedToEnd) {
                stop = mouseEvents.size();
                parentScene.makeMove("forward");
            }

            do { // execute at least once.

                // SceneState tempScene = (SceneState)(vector.getStateVector().elementAt(0));
                // tempScene.componentOpacityActive.updateTransFunc(tempScene.transformFunc);
                for (int i = 0; i < stop; i++) {

                    // get mouse events
                    vector = (MouseEventVector) mouseEvents.elementAt(i);

                    if (i == 0) {
                        parentScene.setCurrentState(vector.getState());
                    }

                    // possible that there's no mouseEvents, as with a view
                    if (vector.getMouseEvents().size() > 0) {

                        // set when the first time through; reset when first record event in series.
                        if ((when < 10000) || (vector.isFirst() == true)) {

                            try {
                                when = ((MouseEvent) vector.getMouseEvents().elementAt(0)).getWhen();
                            } catch (ClassCastException error) { }
                            // set up slices appropriately.
                        }
                    } else {

                        // want when to reset after
                        when = 0;
                    }

                    // if no mouseEvents, this won't execute; otherwise plays the hidden
                    // vector of mouse events.
                    // System.out.println("Number of events in vector = " + vector.getMouseEvents().size());
                    for (int j = 0; j < vector.getMouseEvents().size(); j++) {
                        long diff;
                        EventObject event = (EventObject) vector.getMouseEvents().elementAt(j);

                        diff = time;
                        newWhen = when + time;

                        // a wait(0) will wait forever, so adjust
                        if (diff <= 0) {
                            diff = 1;
                        }

                        source = event.getSource();

                        if (source == rightRotateButton) {
                            parentScene.makeMove("clockwise");
                        } else if (source == leftRotateButton) {
                            parentScene.makeMove("counterclockwise");
                        } else if (source == middleUpButton) {
                            parentScene.makeMove("forward");
                        } else if (source == middleDownButton) {
                            parentScene.makeMove("backward");
                        } else if (source == rightUpButton) {
                            parentScene.makeMove("lookup");
                        } else if (source == rightDownButton) {
                            parentScene.makeMove("lookdown");
                        } else if (source == rightLeftButton) {
                            parentScene.makeMove("lookleft");
                        } else if (source == rightRightButton) {
                            parentScene.makeMove("lookright");
                        }

                        // wait for as long as the user did between mouse events
                        try {

                            synchronized (this) {
                                wait(diff);
                            }
                        } catch (InterruptedException ex) { }

                        when = newWhen;

                        // if paused, sleep until they press play again
                        while (isPaused()) {

                            try {
                                sleep(5L);
                            } catch (InterruptedException ex) { }
                        }

                        // if stopped, quit this thread and reenable the record button
                        if (isStopped()) {
                            stopButton.setSelected(true);
                            recordButton.setEnabled(true);
                            aviRecordButton.setEnabled(true);
                            contButton.setEnabled(true);
                            stepButton.setEnabled(true);
                            playButton.setEnabled(true);
                            isPlaying = false;

                            return;
                        }
                    }

                    // Need to repeat code here in case it's a transform and no mouse events are played.
                    // if paused, sleep until they press play again
                    while (isPaused()) {

                        try {
                            sleep(5L);
                        } catch (InterruptedException ex) { }
                    }

                    // if stopped, quit this thread and reenable the record button
                    if (isStopped()) {
                        stopButton.setSelected(true);
                        recordButton.setEnabled(true);
                        aviRecordButton.setEnabled(true);
                        contButton.setEnabled(true);
                        stepButton.setEnabled(true);
                        playButton.setEnabled(true);
                        isPlaying = false;

                        return;
                    }
                }

                if (forever) {
                    when = 0;
                }
            } while (forever); // infinite loop if true; waiting for stop or pause

            isPlaying = false;

            // when not playing, we're in stop mode, reenable record button
            mode = STOP_MODE;
            stopButton.setSelected(true);
            recordButton.setEnabled(true);
            aviRecordButton.setEnabled(true);
            contButton.setEnabled(true);
            stepButton.setEnabled(true);
            playButton.setEnabled(true);
        }

    }


    /**
     * Helper class which sets the appropriate icons for the right button. Will use "instanceof" keyword to determine
     * icons later.
     */
    class RightButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -2852165225441134233L;

        /**
         * Creates new button with correct icon and given tool tip.
         *
         * @param  tip  Tool tip for button.
         */
        public RightButton(String tip) {
            super(MipavUtil.getIcon("rightarrow.gif"));
            setRolloverIcon(MipavUtil.getIcon("rightarrowroll.gif"));
            setButtonProps(this, tip);
        }
    }


    /**
     * Helper class which sets the appropriate icons for the left button. Will use "instanceof" keyword to determine
     * icons later.
     */
    class RightRotateButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -3655606662884621372L;

        /**
         * Creates new button with correct icon and given tool tip.
         *
         * @param  tip  Tool tip for button.
         */
        public RightRotateButton(String tip) {
            super(MipavUtil.getIcon("rightrotate.gif"));
            setRolloverIcon(MipavUtil.getIcon("rightrotateroll.gif"));
            setButtonProps(this, tip);
        }
    }

    /**
     * Class used to send Standard mouse events to the canvas. Must subclass Thread because a single <code>
     * mousePressed</code> event on one of the mouse buttons needs to generate <code>mouseDragged</code> events on the
     * canvas until the mouse is released.
     */
    class StandardMouse extends Thread {

        /** DOCUMENT ME! */
        MouseEvent currentEvent;

        /** int centerX, centerY;. */
        MouseEvent evt;

        /** DOCUMENT ME! */
        Object source;

        /** DOCUMENT ME! */
        long when;

        /** DOCUMENT ME! */
        int x, y, mod, id;

        /**
         * Creates new thread and sets up mouse event variables appropriately.
         *
         * @param  event  Original mouse event, from button.
         */
        public StandardMouse(MouseEvent event) {
            when = event.getWhen();
            currentEvent = event;
            id = MouseEvent.MOUSE_DRAGGED;
            source = event.getSource();

            if ((source == middleUpButton) || (source == middleDownButton) || (source == middleLeftButton) ||
                    (source == middleRightButton)) {

                // middle mouse button plus whatever extra modifiers were on original event (alt mask, shift mask, etc)
                mod = MouseEvent.BUTTON2_MASK + (event.getModifiers() - MouseEvent.BUTTON1_MASK);
            } else {
                mod = MouseEvent.BUTTON3_MASK;
            }

            if (isRecording()) {
                eventVector.add(event, parentScene.getBranchState());
            }

            evt = new MouseEvent(parentScene.getCanvas(), MouseEvent.MOUSE_PRESSED, when, mod, Math.round(x),
                                 Math.round(y), 1, false);
        }

        /**
         * Runs the thread. While the button is pressed, dispatches mouse dragged events at a rate consistent with the
         * velocity slider. Once the mouse is released, <code>pressed</code> will be set to false and the loop will
         * stop.
         */
        public synchronized void run() {

            while (pressed) {
                parentScene.getCanvas().dispatchEvent(evt);

                if (source == rightRotateButton) {
                    parentScene.makeMove("clockwise");
                } else if (source == leftRotateButton) {
                    parentScene.makeMove("counterclockwise");
                } else if (source == middleUpButton) {
                    parentScene.makeMove("forward");
                } else if (source == middleDownButton) {
                    parentScene.makeMove("backward");
                } else if (source == rightUpButton) {
                    parentScene.makeMove("lookup");
                } else if (source == rightDownButton) {
                    parentScene.makeMove("lookdown");
                } else if (source == rightLeftButton) {
                    parentScene.makeMove("lookleft");
                } else if (source == rightRightButton) {
                    parentScene.makeMove("lookright");
                }

                if (isRecording()) {
                    eventVector.add(currentEvent, parentScene.getBranchState());
                }

                if (isAVIRecording()) {
                    parentScene.writeImage();
                }

                when += time;

                try {

                    // parentScene.rotateImage();
                    wait(time);
                } catch (InterruptedException error) { }

                evt = new MouseEvent(parentScene.getCanvas(), id, when, mod, Math.round(x), Math.round(y), 0, false);

            }

        }
    }


    /**
     * Helper class which sets the appropriate icons for the up button. Will use "instanceof" keyword to determine icons
     * later.
     */
    class UpButton extends JButton {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -5378024239639173836L;

        /**
         * Creates new button with correct icon and given tool tip.
         *
         * @param  tip  Tool tip for button.
         */
        public UpButton(String tip) {
            super(MipavUtil.getIcon("up.gif"));
            setRolloverIcon(MipavUtil.getIcon("uproll.gif"));
            setButtonProps(this, tip);
        }
    }

}
