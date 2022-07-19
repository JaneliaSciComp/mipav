package gov.nih.mipav.view.renderer.J3D.surfaceview.plotterview;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;

import com.sun.j3d.utils.behaviors.mouse.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

import java.io.*;

import java.util.*;

import javax.media.j3d.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import javax.vecmath.*;


/**
 * Mouse recorded dialog. Has the ability to load and save "mouse action files", which contain the information necessary
 * to reconstruct a series of mouse actions that took place on the canvas of the Triplanar/Surface render viewer. The
 * user can also record mouse actions, pause, stop, and play back. Play back can be done one step at a time, for the
 * whole list, or continuously until the stop button is hit. The mouse events show up in a list. The user can also add
 * or remove different views to this list, by pressing the "Add current" button when at a view he/she wants to save.
 * When the "Save file" menu option is chosen, everything in the list is saved, including the different views.
 *
 * @author  Neva Cherniavsky
 */
public class JPanelMousePlotter extends JPanelRendererJ3D
        implements MouseListener, MouseMotionListener, ChangeListener, MouseBehaviorCallback {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8105261993244718879L;

    /** Recorder/Player is stopped. */
    public static final int STOP_MODE = 0;

    /** Recorder/Player is recording. */
    public static final int RECORD_MODE = 1;

    /** Recorder/Player is paused. */
    public static final int PAUSE_MODE = 2;

    /** Recorder/Player is playing. */
    public static final int PLAY_MODE = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** flag to indicate current mode being changed. */
    boolean changedMode = false;
    // boolean              original        = true;

    /** Continue play button. */
    JToggleButton contButton;

    // int boxCount = 1;
    /** index to the item list. */
    int current = 0;

    /** Current event vector used by the save AVI. */
    MouseEventVectorPlotter currentEventVector;
    // MouseEventVectorPlotter    firstEventVector;
    // EventObject         currentEvent;

    /** current SceneState object being recorded by SaveAVI. */
    Object currentObject;

    /** Current transform3D object used in Save AVI. */
    Transform3D currentTransform;

    /** Vector that holds mouse or change events. */
    Vector<MouseEventVectorPlotter> events; 

    /** Global mouse event vector. */
    MouseEventVectorPlotter eventVector;

    /** Flag to indicate first event. */
    boolean first = true;

    /** Go to button. */
    JButton goButton;

    /** Flag to indicating play mouse is running or not. */
    boolean isPlaying = false;

    /** List to record info for each mouse, change events. */
    DefaultListModel listModel;

    /** Current mode, init to stop. */
    int mode = STOP_MODE;

    /** Mouse event counter. */
    int mouseCount = 1;

    /** Save AVI button . */
    JButton movieButton;

    /** Reference to ViewJFrameSurfacePlotter. */
    SurfacePlotter myParentPlotter;

    /** Used by name change dialog to change name of the selected item. */
    MouseEventVectorPlotter nameChangeVector;

    /** This button add at head of the list. */
    JButton newButton;

    /** Parent scene. */
    RenderViewBase parentScene;

    /** Pause button. */
    JToggleButton pauseButton;

    /** Display button. */
    JToggleButton playButton;

    /** Reference to PlayMouse class object. */
    PlayMouse playMouse;

    /** Process id used by the save AVI. */
    int process = -1;

    /** Recorder button. */
    JToggleButton recordButton;

    /** Reference to RecordMouse object class. */
    RecordMouse recorderToAVI = null;

    /** Remove all items in the item list. */
    JButton removeAllButton;

    /** Remove the selected item in the item list. */
    JButton removeButton;

    /** Current transform3D object. */
    Transform3D resetTransform;

    /** Slider moves event counter. */
    int sliderCount = 1;

    /** Next step button. */
    JToggleButton stepButton;

    /** Stop button. */
    JToggleButton stopButton;

    /** Tool bar of the mouse recorder. */
    JToolBar toolBar;

    /** DOCUMENT ME! */
    boolean transformChange = false;

    /** View list of the mouse recorder display list. */
    JList viewList;

    /** DOCUMENT ME! */
    private float mjpegQuality = 0.8f;

    /** flag to indicate the first time mouse tranform event happpened. */
    private boolean setMouseVectorFlag;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a new mouse recorder dialog. Makes the GUI components and ties them to the parent scene.
     *
     * @param  parent  Parent scene to tie actions to.
     */
    public JPanelMousePlotter(RenderViewBase parent) {
        super(parent);

        try {
            events = new Vector<MouseEventVectorPlotter>(); 
            resetTransform = new Transform3D();
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in mouse recorder.");

            return;
        }

        parentScene = parent;

        if (parent instanceof SurfaceRender) {
            myParentPlotter = (SurfacePlotter) renderBase;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Performs various actions depending on which event triggered this method.
     *
     * <p>Playback toolbar buttons:<br>
     * </p>
     *
     * <ul>
     *   <li>Record button - changes the mode to RECORD_MODE.</li>
     *   <li>Play button - changes the mode to PLAY_MODE and calls play(boolean, boolean), which plays all the mouse
     *     events started at the selected one in a separate thread.</li>
     *   <li>Play once button - changes the mode to PLAY_MODE and calls play(boolean, boolean), which plays one mouse
     *     event (the selected mouse event) in a separate thread.</li>
     *   <li>Play continuously button - changes the mode to PLAY_MODE and calls play(boolean, boolean), which plays all
     *     the mouse events started at the selected one in a separate thread, until the stop button or pause button is
     *     pressed.</li>
     *   <li>Pause button - changes the mode to PAUSE_MODE and causes the thread running in play(boolean, boolean) to
     *     sleep.</li>
     *   <li>Stop button - changes the mode to STOP_MODE and causes play(boolean, boolean) to return.</li>
     * </ul>
     *
     * List buttons:<br>
     *
     * <ul>
     *   <li>Add current - brings up a dialog asking the user for a name for the view, then adds it to the list and the
     *     vector.</li>
     *   <li>Go - goes to the view associated with the name selected in the list.</li>
     *   <li>Remove - removes the view associated with the name selected in the list. Disables playback if there are no
     *     views left in the list.</li>
     * </ul>
     *
     * Menu events:<br>
     *
     * <ul>
     *   <li>Load mouse file - loads in an object file previously saved with the "Save mouse file" command. Displays the
     *     views in the list.</li>
     *   <li>Save mouse file - uses the serialized writeObject method to save the vector of views/mouse events to a
     *     file.</li>
     *   <li>Exit - disposes of this dialog.</li>
     * </ul>
     *
     * @param  event  Event that triggered this method.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Save")) {
            JDialogSimpleText text;
            Transform3D t3D;

            // get name for the view
            try {
                text = new JDialogSimpleText(this, renderBase);
                t3D = new Transform3D();
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in mouse recorder.");

                return;
            }

            if (!text.isCancelled()) {

                // get the view
                parentScene.getSceneRootTG().getTransform(t3D);

                // store name and view together
                MouseEventVectorPlotter vector = new MouseEventVectorPlotter(text.getName(), t3D, false,
                                                                             parentScene.getSceneState(),
                                                                             ((SurfacePlotter) parentScene)
                                                                                 .getMouseMode());

                listModel.addElement(text.getName());
                events.add(vector);

                // enable playback buttons because there are elements in the list
                playButton.setEnabled(true);
                contButton.setEnabled(true);
                stepButton.setEnabled(true);
                stopButton.setEnabled(true);
                pauseButton.setEnabled(true);
                goButton.setEnabled(true);
                removeButton.setEnabled(true);
                removeAllButton.setEnabled(true);
            }
        } else if (command.equals("Go")) {
            int index = viewList.getSelectedIndex();

            // if a file is selected
            if (index > -1) {

                // go to view
                parentScene.getSceneRootTG().setTransform(((MouseEventVectorPlotter) events.elementAt(index)).getView());

                // parentScene.updateImages(true);
                if (!listModel.isEmpty()) {
                    play(false, false);
                }
            }
        } else if (command.equals("Remove")) {
            int index = viewList.getSelectedIndex();

            // if a file is selected
            if (index > -1) {

                // remove it from the list and from the vector
                listModel.remove(index);
                events.removeElementAt(index);

                // if no more elements in list, disable playback
                if (listModel.isEmpty()) {
                    removeButton.setEnabled(false);
                    removeAllButton.setEnabled(false);
                } else {

                    if (index >= listModel.getSize()) {
                        index = listModel.getSize() - 1;
                    }

                    viewList.setSelectedIndex(index);
                }
            }
        } else if (command.equals("Remove All")) {
            int index = viewList.getSelectedIndex();

            listModel.removeAllElements();
            events.removeAllElements();

            if (index > -1) {

                if (listModel.isEmpty()) {
                    removeButton.setEnabled(false);
                    removeAllButton.setEnabled(false);
                } else {

                    if (index >= listModel.getSize()) {
                        index = listModel.getSize() - 1;
                    }

                    viewList.setSelectedIndex(index);
                }
            }
        } else if (command.equals("Record")) {

            if (mode != RECORD_MODE) {
                changedMode = true;
                first = true;
            }

            mode = RECORD_MODE;
        } else if (command.equals("Play") && (mode != PLAY_MODE)) {
            changedMode = true;
            mode = PLAY_MODE;

            // disable record while playing, so we don't record the
            // events we're playing
            recordButton.setEnabled(false);
            contButton.setEnabled(false);
            stepButton.setEnabled(false);
            removeButton.setEnabled(false);
            removeAllButton.setEnabled(false);

            if (!listModel.isEmpty()) {
                play(true, false);
            }
        } else if (command.equals("ContPlay") && (mode != PLAY_MODE)) {
            changedMode = true;
            mode = PLAY_MODE;

            // disable record while playing, so we don't record the
            // events we're playing
            recordButton.setEnabled(false);
            playButton.setEnabled(false);
            stepButton.setEnabled(false);

            // removeButton.setEnabled(false);
            // removeAllButton.setEnabled(false);
            if (!listModel.isEmpty()) {
                play(true, true);
            }
        } else if (command.equals("Step") && (mode != PLAY_MODE)) {
            changedMode = true;
            mode = PLAY_MODE;

            // disable record while playing, so we don't record the
            // events we're playing
            recordButton.setEnabled(false);
            contButton.setEnabled(false);
            playButton.setEnabled(false);

            if (!listModel.isEmpty()) {
                play(false, false);
            }
        } else if (command.equals("Pause")) {

            if (mode != PAUSE_MODE) {
                changedMode = true;
            }

            mode = PAUSE_MODE;

            if ((playMouse != null) && playMouse.isAlive()) {
                playMouse.interrupt();
            }
        } else if (command.equals("Stop")) {

            if (mode != STOP_MODE) {
                changedMode = true;
            }

            mode = STOP_MODE;

            if ((playMouse != null) && playMouse.isAlive()) {
                playMouse.interrupt();
            }

            removeButton.setEnabled(true);
            removeAllButton.setEnabled(true);
            playButton.setEnabled(true);
            contButton.setEnabled(true);
            stepButton.setEnabled(true);
            pauseButton.setEnabled(true);
            goButton.setEnabled(true);
            recordButton.setEnabled(true);
        } else if (command.equals("SaveFile")) {

            try {
                save();
            } catch (IOException e) {
                e.printStackTrace();
                MipavUtil.displayError("Error while trying to save mouse file.");
            }
        } else if (command.equals("SaveAVI")) {
            /*
             * mode = STOP_MODE; stopButton.setSelected(true); try {  saveAVI(); } catch (IOException e) {
             * MipavUtil.displayError("Error while trying to save mouse file."); }
             */
        } else if (command.equals("LoadFile")) {

            try {
                load();
            } catch (Exception e) {
                MipavUtil.displayError("Error while trying to load mouse file.");
            }
        } else if (command.equals("X") || command.equals("Y") || command.equals("Z")) { // need to save state.  need to
                                                                                        // debug slider problems.  I
                                                                                        // think it's because
                                                                                        // stateChanged is getting

            // the value from the slider, which isn't changing.  but the text box is??  should move slider maybe.
            /*
             * if (mode == RECORD_MODE) { Transform3D t3D = new Transform3D(); // get the view
             * parentScene.getSceneRootTG().getTransform(t3D); // store name and view together MouseEventVectorPlotter
             * vector = new MouseEventVectorPlotter(command + " slice" + boxCount, t3D, first,
             * parentScene.getSceneState(), parentScene.getMouseMode()); if (first == true) { first = false; }
             * vector.add(event); listModel.addElement(command + " slice" + boxCount); boxCount++; events.add(vector); }
             */
        } else if (command.equals("Exit")) {

            if (isPlaying) {
                playMouse.requestStop();
                isPlaying = false;
            }

            events = new Vector();
            ((SurfacePlotter) renderBase).resetMouseDialog();
            dispose();
        }
    }

    /**
     * Dispose memory.
     */
    public void dispose() {
        System.out.println("JDialogMouseRecorderPlotter");
        parentScene = null;

        if (events != null) {
            events.removeAllElements();
            events = null;
        }

        resetTransform = null;
        currentTransform = null;

        if (viewList != null) {
            viewList.removeAll();
            viewList = null;
        }

        if (listModel != null) {
            listModel.removeAllElements();
            listModel = null;
        }

        if (recorderToAVI != null) {
            recorderToAVI.disposeLocal();
            recorderToAVI = null;
        }

        if (playMouse != null) {
            playMouse.requestStop();
            playMouse = null;
        }

        if (currentEventVector != null) {
            currentEventVector = null;
        }

        if (nameChangeVector != null) {
            nameChangeVector = null;
        }

        myParentPlotter = null;

        if (eventVector != null) {
            eventVector = null;
        }
        // super.dispose();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseClicked(MouseEvent event) {
        Object source = event.getSource();
        String nameChanged;

        if ((source == viewList) && (event.getClickCount() >= 2)) { // mouse double click

            // handle text change event for Mouse and Slider name.
            int index = viewList.locationToIndex(event.getPoint());

            if (index < 0) {
                return;
            }

            nameChangeVector = (MouseEventVectorPlotter) events.elementAt(index);

            ChangeNameDialog nameDialog = new ChangeNameDialog(renderBase);

            nameChanged = nameDialog.getName();
            nameChangeVector.setName(nameChanged);
            listModel.setElementAt(nameChanged, index);
        }
    }

    /**
     * If recording, adds this mouse event to the mouseEvents vector found at location <code>current</code>.
     *
     * @param  event  Original mouse event.
     */
    public void mouseDragged(MouseEvent event) {

        if ((mode == RECORD_MODE) && transformChange) {

            // go to the proper MouseEventVectorPlotter           and add this mouseEvent to the vector in there
            // ( (MouseEventVectorPlotter) events.elementAt(current)).add(event, parentScene.getSceneState());
            eventVector.add(event, parentScene.getSceneState());
        }
    }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseEntered(MouseEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseExited(MouseEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseMoved(MouseEvent event) { }

    /**
     * If mode is RECORD_MODE, saves the current view and creates a new MouseEventVectorPlotter. That object holds the
     * name, the view, and a vector with the subsequent mouse events. This is so the size of the list that the user sees
     * stays small. If all mouse events were displayed in the list, the list would soon reach into the thousands. This
     * way, the user sees only the "beginning" of the mouse event. When playback happens, all the mouse events stored in
     * the vector within MouseEventVectorPlotter are played. For more information, see MouseEventVectorPlotter.
     *
     * @param  event  Original mouse event.
     */
    public void mousePressed(MouseEvent event) {

        if (mode == RECORD_MODE) {
            // MouseEventVectorPlotter vector;

            try {
                Transform3D t3d = new Transform3D();

                // save current view
                parentScene.getSceneRootTG().getTransform(t3d);

                // save under name "Mouse0" , "Mouse36", etc.
                eventVector = new MouseEventVectorPlotter("Mouse " + mouseCount, t3d, first,
                                                          parentScene.getSceneState(),
                                                          ((SurfacePlotter) parentScene).getMouseMode());

                if (first == true) {
                    first = false;
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in mouse recorder.");

                return;
            }

            setMouseVectorFlag = true;

            eventVector.add(event, parentScene.getSceneState());

            // save the current location of this MouseEventVectorPlotter in the larger events vector.
            // this is so MouseDragged puts its events in the proper place.
            // events.add(eventVector);
            // current = events.indexOf(eventVector);
            // if list was empty before, it isn't now, so enable playback.
            playButton.setEnabled(true);
            contButton.setEnabled(true);
            stepButton.setEnabled(true);
            stopButton.setEnabled(true);
            pauseButton.setEnabled(true);
            goButton.setEnabled(true);
            removeButton.setEnabled(true);
            removeAllButton.setEnabled(true);
        }
    }

    /**
     * If recording, adds this mouse event to the mouseEvents vector found at location <code>current</code>.
     *
     * @param  event  Original mouse event.
     */
    public void mouseReleased(MouseEvent event) {

        if ((mode == RECORD_MODE) && transformChange) {
            // go to the proper MouseEventVectorPlotter           and add this mouseEvent to the vector in there
            // ((MouseEventVectorPlotter)events.elementAt(current)).add(event); ( (MouseEventVectorPlotter)
            // events.elementAt(current)).add(event, parentScene.getSceneState());

            eventVector.add(event, parentScene.getSceneState());
            events.add(eventVector);
            mouseCount++;
        }

        transformChange = false;
    }

    /**
     * Removes all the items on the recording list.
     */
    public void removeAllItems() {
        int index = viewList.getSelectedIndex();

        listModel.removeAllElements();
        events.removeAllElements();

        if (index > -1) {

            if (listModel.isEmpty()) {
                removeButton.setEnabled(false);
                removeAllButton.setEnabled(false);
            } else {

                if (index >= listModel.getSize()) {
                    index = listModel.getSize() - 1;
                }

                viewList.setSelectedIndex(index);
            }
        }
    }

    /**
     * Sets up variables for recorder.
     */
    public void setup() {
        parentScene.getSceneRootTG().getTransform(resetTransform);
        parentScene.getCanvas().addMouseListener(this);
        parentScene.getCanvas().addMouseMotionListener(this);
        init();
    }

    /**
     * Makes the dialog visible next to the parent frame. If this makes it go off the screen, puts the dialog in the
     * center of the screen.
     *
     * @param  status  Flag indicating if the dialog should be visible.
     */
    public void setVisible(boolean status) {
        Point location = new Point();

        location.x = renderBase.getLocation().x;
        location.y = renderBase.getLocation().y + renderBase.getHeight();

        if (((location.x + getWidth()) < Toolkit.getDefaultToolkit().getScreenSize().width) &&
                ((location.y + getHeight()) < Toolkit.getDefaultToolkit().getScreenSize().height) &&
                (location.x != 0) && (location.y != 0)) {
            setLocation(location);
        } else {
            Rectangle dialogBounds = getBounds();

            setLocation((Toolkit.getDefaultToolkit().getScreenSize().width / 2) - (dialogBounds.width / 2),
                        (Toolkit.getDefaultToolkit().getScreenSize().height / 2) - (dialogBounds.height / 2));
        }

        super.setVisibleStandard(status);
    }

    /**
     * Sets how the image plane should be displayed depending on value of slider.
     *
     * @param  e  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) {

        if (mode == RECORD_MODE) {
            // if list was empty before, it isn't now, so enable playback.

            playButton.setEnabled(true);
            contButton.setEnabled(true);
            stepButton.setEnabled(true);
            stopButton.setEnabled(true);
            pauseButton.setEnabled(true);
            goButton.setEnabled(true);
            removeButton.setEnabled(true);
            removeAllButton.setEnabled(true);

        }
    }

    /**
     * Accessor that lets the dialog know that the canvas transform has changed.
     *
     * @param  type       DOCUMENT ME!
     * @param  transform  DOCUMENT ME!
     */
    public void transformChanged(int type, Transform3D transform) {

        if (MouseBehaviorCallback.ZOOM == type) {

            if ((mode == RECORD_MODE) && setMouseVectorFlag) {
                transformChange = true;
                eventVector.setName("MouseZoom" + mouseCount);
                listModel.addElement("MouseZoom" + mouseCount);
                setMouseVectorFlag = false;
            }
        } else if (MouseBehaviorCallback.ROTATE == type) {

            if ((mode == RECORD_MODE) && setMouseVectorFlag) {
                transformChange = true;
                eventVector.setName("MouseRotate" + mouseCount);
                listModel.addElement("MouseRotate" + mouseCount);
                setMouseVectorFlag = false;
            }
        } else if (MouseBehaviorCallback.TRANSLATE == type) {

            if ((mode == RECORD_MODE) && setMouseVectorFlag) {
                transformChange = true;
                eventVector.setName("MouseTranslate" + mouseCount);
                listModel.addElement("MouseTranslate" + mouseCount);
                setMouseVectorFlag = false;
            }
        }

        synchronized (this) {

            // original = true;
            // process = 1;
            currentTransform = transform;

            if (recorderToAVI != null) {
                recorderToAVI.notify();
            }
        }
    }

    /**
     * Accessor that closing the mouse recorder window frame.
     *
     * @param  e  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent e) {

        if (isPlaying) {

            // stop the current thread when the mouse recorder window is closing
            playMouse.requestStop();
            isPlaying = false;
        }

        events = new Vector();
        ((SurfacePlotter) renderBase).resetMouseDialog();
        dispose();
    }

    /**
     * Calls dispose.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        dispose();
        super.finalize();
    }

    /**
     * Builds menu with load, save, and exit options.
     */
    private void buildMenu() {
        JMenu fileMenu;
        JMenuItem itemLoad, itemSave, itemAVI, itemExit;
        JMenuBar menuBar;

        try {
            fileMenu = new JMenu("File");
            itemLoad = new JMenuItem("Load mouse file");
            itemSave = new JMenuItem("Save mouse file");
            itemAVI = new JMenuItem("Save as AVI");
            itemExit = new JMenuItem("Exit");
            menuBar = new JMenuBar();
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in mouse recorder.");

            return;
        }

        fileMenu.setFont(MipavUtil.font12B);

        itemLoad.addActionListener(this);
        itemLoad.setActionCommand("LoadFile");
        itemLoad.setFont(MipavUtil.font12B);
        fileMenu.add(itemLoad);

        itemSave.addActionListener(this);
        itemSave.setActionCommand("SaveFile");
        itemSave.setFont(MipavUtil.font12B);
        fileMenu.add(itemSave);

        itemAVI.addActionListener(this);
        itemAVI.setActionCommand("SaveAVI");
        itemAVI.setFont(MipavUtil.font12B);
        fileMenu.add(itemAVI);

        fileMenu.addSeparator();

        itemExit.addActionListener(this);
        itemExit.setActionCommand("Exit");
        itemExit.setFont(MipavUtil.font12B);
        fileMenu.add(itemExit);

        menuBar.add(fileMenu);
        // setJMenuBar(menuBar);
    }

    /**
     * Builds the toolbar for the mouse recorder.
     *
     * @return  DOCUMENT ME!
     */
    private JToolBar buildToolBar() {
        JToolBar toolBar;
        ButtonGroup group;
        TitledBorder title;
        Insets insets;
        JButton separator;

        try {
            toolBar = new JToolBar();
            group = new ButtonGroup();
            title = buildTitledBorder("Mouse recorder");
            recordButton = new JToggleButton(MipavUtil.getIcon("record.gif"), false);
            playButton = new JToggleButton(MipavUtil.getIcon("play.gif"), false);
            contButton = new JToggleButton(MipavUtil.getIcon("rightcont.gif"), false);
            stepButton = new JToggleButton(MipavUtil.getIcon("step.gif"), false);
            pauseButton = new JToggleButton(MipavUtil.getIcon("pause.gif"), false);
            stopButton = new JToggleButton(MipavUtil.getIcon("stop.gif"), true);
            movieButton = new JButton(MipavUtil.getIcon("movie.gif"));
            insets = new Insets(0, 0, 0, 0);
            separator = new JButton(MipavUtil.getIcon("separator.gif"));
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in mouse recorder.");

            return null;
        }

        toolBar.setBorder(title);
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);

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

        separator.setBorderPainted(false);
        separator.setFocusPainted(false);
        toolBar.add(separator);
        movieButton.addActionListener(this);
        movieButton.setMargin(insets);
        movieButton.setToolTipText("Save as AVI");
        movieButton.setActionCommand("SaveAVI");
        movieButton.setContentAreaFilled(false);
        toolBar.add(movieButton);

        toolBar.setFloatable(false);

        return toolBar;
    }

    /**
     * Initializes GUI components.
     */
    private void init() {
        MouseEventVectorPlotter vector;
        BorderLayout border1, border2, border3;
        TitledBorder title;
        JScrollPane scrollPane;
        JPanel scrollPanel;
        JPanel newPanel;
        JPanel viewPanel;
        JPanel toolPanel;
        // JPanel mainPanel;

        try {
            vector = new MouseEventVectorPlotter("Original view", resetTransform, true, parentScene.getSceneState(),
                                                 ((SurfacePlotter) parentScene).getMouseMode());
            listModel = new DefaultListModel();
            viewList = new JList(listModel);
            scrollPane = new JScrollPane(viewList);
            scrollPanel = new JPanel();
            goButton = new JButton("Go to");
            newButton = new JButton("Add current");
            removeButton = new JButton("Remove");
            removeAllButton = new JButton("Remove All");
            newPanel = new JPanel(new GridLayout(2, 2));
            viewPanel = new JPanel();
            toolPanel = new JPanel();
            mainPanel = new JPanel();
            border1 = new BorderLayout();
            border2 = new BorderLayout();
            border3 = new BorderLayout();
            title = buildTitledBorder("View");
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in mouse recorder.");

            return;
        }

        // setTitle("Mouse action recorder and viewer");

        buildMenu();
        events.add(vector);

        listModel.addElement("Original view");
        viewList.setPrototypeCellValue("aaaaaaaaaaaaaa");
        viewList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        viewList.setVisibleRowCount(5);
        viewList.addMouseListener(this);

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(scrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        goButton.addActionListener(this);
        goButton.setActionCommand("Go");
        goButton.setFont(serif12B);
        goButton.setPreferredSize(new Dimension(150, 30));
        goButton.setMinimumSize(MipavUtil.defaultButtonSize);

        newButton.addActionListener(this);
        newButton.setActionCommand("Save");
        newButton.setFont(serif12B);
        newButton.setPreferredSize(new Dimension(150, 30));
        newButton.setMinimumSize(MipavUtil.defaultButtonSize);

        removeButton.addActionListener(this);
        removeButton.setActionCommand("Remove");
        removeButton.setFont(serif12B);
        removeButton.setPreferredSize(new Dimension(150, 30));
        removeButton.setMinimumSize(MipavUtil.defaultButtonSize);

        removeAllButton.addActionListener(this);
        removeAllButton.setActionCommand("Remove All");
        removeAllButton.setFont(serif12B);
        removeAllButton.setPreferredSize(new Dimension(150, 30));
        removeAllButton.setMinimumSize(MipavUtil.defaultButtonSize);

        newPanel.add(goButton);
        newPanel.add(newButton);
        newPanel.add(removeButton);
        newPanel.add(removeAllButton);

        viewPanel.setLayout(border1);
        viewPanel.add(scrollPanel);
        viewPanel.add(newPanel, BorderLayout.SOUTH);
        viewPanel.setBorder(title);

        toolPanel.setLayout(border2);
        toolBar = buildToolBar();
        toolPanel.add(toolBar);

        mainPanel.setLayout(border3);
        mainPanel.add(toolPanel, BorderLayout.NORTH);
        mainPanel.add(viewPanel);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        // getContentPane().add(mainPanel);
        // pack();
    }

    /**
     * Saves the mouse actions as an AVI file. Brings up a dialog for the user to choose a file name and directory, then
     * writes out the AVI file.
     *
     * <p>The AVI file will be large, so it is opened and sent the data in pieces. In order to mimic the time delay that
     * can occur between mouse events, blank frames are sent. To make this precise, and also to save space, frames are
     * only written once every 143 milliseconds. This translates into a frame rate of approximately 7 frames per second.
     * </p>
     *
     * <p>Here is how it works:</p>
     *
     * <ol>
     *   <li>As in play(), we cycle through each mouse event by going through each MouseEventVectorPlotter and looking
     *     at each mouse event stored in the hidden vector within each MouseEventVectorPlotter.</li>
     *   <li>There is also a counter that keeps track of how many milliseconds have elapsed by looking at the
     *     event.getWhen() variable in a MouseEvent.</li>
     *   <li>Once every 143 milliseconds, a frame is recorded. If the image that is currently on the canvas has not been
     *     recorded, it is captured and processed, and a data frame is sent to FileAvi. If it has already been recorded,
     *     a blank frame is sent to FileAvi. So for example, if the user paused for 500 milliseconds between mouse
     *     events, one data frame would be sent, followed by two blank frames. Then when someone played back the AVI
     *     file, it would look like the user paused for that amount of time.</li>
     *   <li>The AVI file knows how many frames should be sent and once they've all been sent, it writes out the end
     *     signature and closes the file.</li>
     * </ol>
     *
     * <p>User may control size of AVI by changing the frame rate in the dialog or the size of the image stored. The
     * frame rate is defined as something like 7 frames per second, which for our purposes means 7 frames per 1000
     * milliseconds. Therefore the interval at which frames are saved would be the inverse of the frame rate - in this
     * example, once every (1000/7) milliseconds ~ 143 milliseconds.</p>
     *
     * @throws  IOException  DOCUMENT ME!
     */
    /*
     * private synchronized void saveAVI() throws IOException { double frameRate = 7d; int subSample = 1;
     * JDialogSurfaceAVI aviDialog = new JDialogSurfaceAVI(renderBase);
     *
     * if (aviDialog.isCancelled()) {     return; } else {     frameRate = aviDialog.getFrameRate();     subSample =
     * aviDialog.getSubSample(); }
     *
     * String fileName; String directory;
     *
     * JFileChooser chooser;
     *
     * try {     chooser = new JFileChooser();     chooser.addChoosableFileFilter(             new ViewImageFileFilter(new
     * String[] { "avi" }));     if (((ViewJFrameBase) parentFrame).getUserInterface().getDefaultDirectory()
     * != null) {         chooser.setCurrentDirectory(                 new File(
     * ((ViewJFrameBase) parentFrame).getUserInterface().getDefaultDirectory()));     } else {
     * chooser.setCurrentDirectory(                 new File(System.getProperties().getProperty("user.dir")));     } }
     * catch (OutOfMemoryError e) {     MipavUtil.displayError("Out of memory in mouse recorder.");     return; } int
     * returnVal = chooser.showSaveDialog(parentFrame);
     *
     * if (returnVal == JFileChooser.APPROVE_OPTION) {     fileName = chooser.getSelectedFile().getName();     directory =
     * String.valueOf(chooser.getCurrentDirectory())             + File.separatorChar;     ((ViewJFrameBase)
     * parentFrame).getUserInterface().setDefaultDirectory(             directory); } else {     return; }
     *
     * try {     recorderToAVI = new RecordMouse(subSample, frameRate,             new FileAvi(((ViewJFrameBase)
     *   parentFrame).getUserInterface(), fileName, directory),             ((ViewJFrameBase)
     * parentFrame).getUserInterface());     recorderToAVI.start();     recorderToAVI = null; } catch (OutOfMemoryError
     * e) {     MipavUtil.displayError("Out of memory in mouse recorder.");     return; } }
     */
    /**
     * Loads the mouse actions as an object file. Clears out the old list and vector, brings up a dialog for the user to
     * choose a file name and directory, then reads in each MouseEventVectorPlotter and puts it in the events vector
     * (and puts the name in the list).
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void load() throws IOException {
        String fileName;
        String directory;

        JFileChooser chooser;

        try {
            chooser = new JFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in mouse recorder.");

            return;
        }

        int returnVal = chooser.showOpenDialog(renderBase);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            ViewUserInterface.getReference().setDefaultDirectory(directory);
        } else {
            return;
        }

        events.removeAllElements();
        listModel.removeAllElements();

        FileInputStream in;
        ObjectInputStream instream;

        try {
            in = new FileInputStream(directory + fileName);
            instream = new ObjectInputStream(in);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in mouse recorder.");

            return;
        }

        try {

            while (true) {
                MouseEventVectorPlotter object = (MouseEventVectorPlotter) instream.readObject();

                events.add(object);
                listModel.addElement(object.name);
            }
        } catch (EOFException error) { }
        catch (ClassNotFoundException error) { }

        in.close();
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
     * Saves the mouse actions as an object file. Brings up a dialog for the user to choose a file name and directory,
     * then writes out the events vector using an ObjectOutputStream.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void save() throws IOException {
        String fileName;
        String directory;

        JFileChooser chooser;

        try {
            chooser = new JFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in mouse recorder.");

            return;
        }

        int returnVal = chooser.showSaveDialog(renderBase);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            ViewUserInterface.getReference().setDefaultDirectory(directory);
        } else {
            return;
        }

        FileOutputStream o;
        ObjectOutputStream ostream;

        try {
            o = new FileOutputStream(directory + fileName);
            ostream = new ObjectOutputStream(o);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in mouse recorder.");

            return;
        }

        for (Enumeration e = events.elements(); e.hasMoreElements();) {
            ostream.writeObject(e.nextElement());
        }

        ostream.flush();
        o.close();
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Change the name of the selected item in the item list.
     */
    class ChangeNameDialog extends JDialogBase {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 4614935574889646920L;

        /** DOCUMENT ME! */
        private JPanel buttonPanel;

        /** DOCUMENT ME! */
        private JButton cancelButton;

        /** DOCUMENT ME! */
        //private JDialog changeDialog;

        /** DOCUMENT ME! */
        private JLabel message;

        /** DOCUMENT ME! */
        private JButton okButton;

        /** DOCUMENT ME! */
        private String textChanged;

        /** DOCUMENT ME! */
        private JTextField textField;

        /** DOCUMENT ME! */
        private JPanel textPanel;

        /**
         * Constructor of change name dialog.
         *
         * @param  parent  Parent Frame.
         */
        public ChangeNameDialog(RenderViewBase parent) {

            // super(parent, true);
            setTitle("Name Change Dialog");
            textChanged = nameChangeVector.getName();
            textField = new JTextField(textChanged, 15);
            okButton = new JButton("Ok");
            okButton.addActionListener(this);
            cancelButton = new JButton("Cancel");
            cancelButton.addActionListener(this);
            buttonPanel = new JPanel();
            buttonPanel.add(okButton);
            buttonPanel.add(cancelButton);
            textPanel = new JPanel();
            textPanel.add(textField);
            message = new JLabel("Please enter the name:");
            getContentPane().add(message, BorderLayout.NORTH);
            getContentPane().add(textPanel, BorderLayout.CENTER);
            getContentPane().add(buttonPanel, BorderLayout.SOUTH);
            setSize(230, 100);
            setResizable(false);
            pack();
            this.setVisible(true);
        }

        /**
         * Perform action for okButton and cancelButton.
         *
         * @param  event  Event that triggered function
         */
        public void actionPerformed(ActionEvent event) {
            Object source = event.getSource();

            if (source == okButton) {
                textChanged = textField.getText();
                this.setVisible(false);
            } else if (source == cancelButton) {
                textChanged = nameChangeVector.getName();
                this.setVisible(false);
            } else {
                super.actionPerformed(event);
            }
        }

        /**
         * Get the text field string.
         *
         * @return  DOCUMENT ME!
         */
        public String getName() {
            return textChanged;
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
        boolean isStopRequested = false;

        /** DOCUMENT ME! */
        boolean selectedToEnd;

        /**
         * Constructs a new thread and initializes the parameters which dictate what kind of play this is.
         *
         * @param  selectedToEnd  <code>true</code> indicates play the list of mouse events in sequential order from the
         *                        first selected index. <code>false</code> indicates only play the selected mouse event.
         * @param  forever        <code>true</code> indicates keep playing mouse events until the stop or pause button
         *                        is pressed.
         */
        public PlayMouse(boolean selectedToEnd, boolean forever) {
            this.selectedToEnd = selectedToEnd;
            this.forever = forever;
            isPlaying = true;
        }

        /**
         * Ask for this thread to stop playback of the events.
         */
        public void requestStop() {
            isStopRequested = true;
        }

        /**
         * Runs the thread and plays according to the established parameters.
         */
        public void run() {
            process = -1;

            int stop;
            MouseEventVectorPlotter vector;
            boolean firstTimePlay = false;
            Transform3D initTransform;
            long when = 0;
            long newWhen = 0;
            int mouseMode = ((SurfacePlotter) parentScene).getMouseMode();
            int index = viewList.getSelectedIndex();

            // if nothing is selected, start at 0.
            if (index == -1) {
                index = 0;
            }

            // if full list, play until end of list; otherwise just play one event.
            if (selectedToEnd) {
                stop = events.size();
            } else {
                stop = index + 1;
            }

            // System.out.println("Number of events = " + events.size());
            firstTimePlay = true;

            do { // execute at least once.

                // SceneState tempScene = (SceneState)(vector.getStateVector().elementAt(0));
                // tempScene.componentOpacityActive.updateTransFunc(tempScene.transformFunc);
                for (int i = index; i < stop; i++) {

                    // set which event we're executing
                    viewList.setSelectedIndex(i);

                    // get mouse events
                    vector = (MouseEventVectorPlotter) events.elementAt(i);
                    currentEventVector = (MouseEventVectorPlotter) events.elementAt(i);

                    // set transform to proper beginning transform for mouse events
                    if (vector.getName().substring(0, 5).equals("Mouse")) {

                        // System.out.println("yes");
                        parentScene.getSceneRootTG().setTransform(vector.getView());
                        // ruida ( (ViewJFrameSurfacePlotter) (parentScene)).updateCubicTransform(vector.getView());
                    }

                    if (vector.isFirst() == true) {
                        ((SurfacePlotter) parentScene).setMouseMode(vector.getMode());

                        /*
                         * if (vector.getStateVector().size() != 0) { SceneState tempScene = (SceneState)
                         * (vector.getStateVector().elementAt(0)); }
                         */
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

                        // System.out.println("Number events vector = " + j);
                        long diff;
                        EventObject event = (EventObject) vector.getMouseEvents().elementAt(j);

                        if (vector.getMouseEvents().elementAt(j) instanceof MouseEvent) {
                            process = 1;
                            currentObject = vector.getStateVector().elementAt(j); // NEW
                            diff = ((MouseEvent) vector.getMouseEvents().elementAt(j)).getWhen() - when;

                            if (j == 0) {
                                diff = 1;
                            }

                            newWhen = ((MouseEvent) vector.getMouseEvents().elementAt(j)).getWhen();
                            ((SurfacePlotter) parentScene).setSceneState(vector.getStateVector().elementAt(j));
                            parentScene.setGUI(vector.getStateVector().elementAt(j));
                        } else {
                            process = 2;
                            diff = 100L;
                            newWhen = when + 100L;
                            currentObject = vector.getStateVector().elementAt(j); // NEW
                            ((SurfacePlotter) parentScene).setSceneState(vector.getStateVector().elementAt(j));
                            parentScene.setGUI(vector.getStateVector().elementAt(j));
                        }

                        // a wait(0) will wait forever, so adjust
                        if (diff <= 0) {
                            diff = 1;
                        }

                        // wait for as long as the user did between mouse events
                        try {

                            synchronized (this) {
                                wait(diff);
                            }
                        } catch (InterruptedException ex) { }

                        // send the canvas the saved mouse event
                        if (firstTimePlay) {
                            initTransform = new Transform3D();
                            initTransform.setScale(0.45f);
                            initTransform.setRotation(new AxisAngle4f(1, 1, 1, (float) .52));

                            // Manually do a transform moving.  Solve the mouse rotation not moving problem.
                            ((SurfacePlotter) parentScene).dispatchSavedEvent(event);
                            transformChanged(MouseBehaviorCallback.ROTATE, initTransform);
                            firstTimePlay = false;
                        }

                        ((SurfacePlotter) parentScene).dispatchSavedEvent(event);
                        when = newWhen;

                        // if paused, sleep until they press play again
                        while (mode == PAUSE_MODE) {

                            try {
                                sleep(5L);
                            } catch (InterruptedException ex) { }
                        }

                        // if stopped, quit this thread and reenable the record button
                        if (mode == STOP_MODE) {
                            stopButton.setSelected(true);
                            recordButton.setEnabled(true);
                            contButton.setEnabled(true);
                            stepButton.setEnabled(true);
                            playButton.setEnabled(true);
                            isPlaying = false;

                            return;
                        }
                    }

                    // Need to repeat code here in case it's a transform and no mouse events are played.
                    // if paused, sleep until they press play again
                    while (mode == PAUSE_MODE) {

                        try {
                            sleep(5L);
                        } catch (InterruptedException ex) { }
                    }

                    // if stopped, quit this thread and reenable the record button
                    if (mode == STOP_MODE) {
                        stopButton.setSelected(true);
                        recordButton.setEnabled(true);
                        contButton.setEnabled(true);
                        stepButton.setEnabled(true);
                        playButton.setEnabled(true);
                        isPlaying = false;

                        return;
                    }
                }

                if (!selectedToEnd) { // stepping through single event

                    // error checking - basically, make the next event in the queue the selected
                    // event, and if we're at the end of the list, make it the top of the list.
                    if (stop >= events.size()) {
                        viewList.setSelectedIndex(0);
                    } else {
                        viewList.setSelectedIndex(stop);
                    }
                }

                if (forever) {
                    index = 0;
                    when = 0;
                }
            } while (forever && !isStopRequested); // infinite loop if true; waiting for stop or pause

            isPlaying = false;

            // when not playing, we're in stop mode, reenable record button
            mode = STOP_MODE;
            stopButton.setSelected(true);
            recordButton.setEnabled(true);
            contButton.setEnabled(true);
            stepButton.setEnabled(true);
            playButton.setEnabled(true);
            removeButton.setEnabled(true);
            removeAllButton.setEnabled(true);
            ((SurfacePlotter) parentScene).setMouseMode(mouseMode);
        }

    }


    /**
     * Thread that records screen capture. Must be a thread so that when taking screen captures at intervals, will know
     * if it's an original image or not. The original flag is set by transformChanged, which listens for transform
     * events from the canvas.
     */

    class RecordMouse extends Thread {

        /** DOCUMENT ME! */
        FileAvi aviFile;

        /** DOCUMENT ME! */
        int blank;

        /** DOCUMENT ME! */
        short[] buffer;

        /** DOCUMENT ME! */
        boolean cancelPressed = false;

        /** DOCUMENT ME! */
        int compression = 0;

        /** DOCUMENT ME! */
        int data;

        /** DOCUMENT ME! */
        int[] extents;

        /** DOCUMENT ME! */
        ModelImage image;

        /** DOCUMENT ME! */
        Image imagePix = null;

        /** DOCUMENT ME! */
        int interval;

        /** DOCUMENT ME! */
        ImageComponent2D javaImage;

        /** DOCUMENT ME! */
        int[] pixels;

        /** DOCUMENT ME! */
        ViewJProgressBar progress;

        /** DOCUMENT ME! */
        javax.media.j3d.Raster raster = null;

        /** DOCUMENT ME! */
        boolean set;

        /** DOCUMENT ME! */
        int xDim;

        /** DOCUMENT ME! */
        int xScale;

        /** DOCUMENT ME! */
        int yDim;

        /** DOCUMENT ME! */
        int yScale;

        /** DOCUMENT ME! */
        int zDim;

        /**
         * Constructor that sets up the variables needed for the screen capture and allocates the buffers appropriately.
         *
         * @param  subSample  Subsample size; determines if we need to scale the image captured.
         * @param  frameRate  Frame rate. Determines how often we capture the images.
         * @param  aviFile    AVI file to write to.
         */
        public RecordMouse(int subSample, double frameRate, FileAvi aviFile) {
            this.aviFile = aviFile;
            set = false; // if we've called "setAVIWrite" this is true; basically tests if we've written the header yet
            zDim = 0; // total number of frames, blank + data
            xDim = parentScene.getCanvas().getWidth();
            yDim = parentScene.getCanvas().getHeight();
            xScale = xDim / subSample; // scaled image xDim
            yScale = yDim / subSample; // scaled image yDim

            JDialogAVIChoice choice = new JDialogAVIChoice(ViewUserInterface.getReference().getMainFrame(), true);

            if (!choice.okayPressed()) {
                cancelPressed = true;

                return;
            }

            compression = choice.getCompression();

            if (compression == AlgorithmTranscode.TRANSCODE_MJPG) {
                mjpegQuality = choice.getMJPEGQuality();
            }

            // must make X and Y multiples of 4 (for encoding)
            if (compression != 0) {
                xScale -= xScale % 4;
                yScale -= yScale % 4;
            }

            interval = (int) Math.round(1000 / frameRate); // interval, take frame when....

            try {
                extents = new int[2]; // extents needed for ModelImage construction
                extents[0] = xScale;
                extents[1] = yScale;
                javaImage = new ImageComponent2D(ImageComponent.FORMAT_RGB, xDim, yDim); // needed to capture screen
                pixels = new int[xScale * yScale];
                buffer = new short[pixels.length * 4];
                image = new ModelImage(ModelImage.ARGB, extents, "AVI");
                raster = new javax.media.j3d.Raster(new Point3f(), javax.media.j3d.Raster.RASTER_COLOR, 0, 0, xDim,
                                                    yDim, javaImage, null);
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in mouse recorder.");

                return;
            }

        }

        /**
         * Dispose memory.
         */
        public void disposeLocal() {
            javaImage = null;
            extents = null;
            pixels = null;
            buffer = null;
            image = null;
            raster = null;
            imagePix = null;
        }

        /**
         * Runs the mouse recorder. Waits for the determined interval (based on frame rate) and then checks to see if
         * this is an original image. The original flag is set by the transformChanged function, which listens for
         * changes in the transform from the canvas. If the transform has changed, then original is set to true, and
         * this method knows about it because it's in a separate thread. Instead of capturing the screen on the play,
         * this method keeps track of the transform. This is because capturing the screen would take too long and the
         * frame rate would no longer be accurate. Then once the playing is done, this method goes through all the
         * transforms and captures the screen or sends a blank frame.
         */
        public void run() {

            // cancel was pressed from the AVIChoice dialog
            if (cancelPressed) {
                return;
            }

            Vector transformVector;
            PlayMouse player;

            // original = true;
            // process = 0;
            viewList.setSelectedIndex(0);
            mode = PLAY_MODE;

            try {
                player = new PlayMouse(true, false);
                transformVector = new Vector();
                progress = new ViewJProgressBar("Saving as AVI", "Playing back", 0, 100, false, null, null);
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in mouse recorder.");

                return;
            }

            progress.updateValue(0, true);
            progress.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50);

            progress.setVisible(true);
            player.start();

            synchronized (this) {

                while (isPlaying) {

                    try {
                        wait(interval); // Frame rate
                    } catch (InterruptedException error) { }

                    // System.out.println(" process = " + process);
                    if (process == -1) { }
                    else if (process == 0) {
                        blank++;
                        transformVector.add("Blank");
                    } else if (process == 1) {
                        ((SceneStatePlotter) (currentObject)).transform = new Transform3D();
                        ((SceneStatePlotter) (currentObject)).transform.set(currentTransform);
                        transformVector.add(currentObject);
                        data++;
                        process = 0;

                    } else if (process == 2) {
                        transformVector.add(currentObject);
                        data++;
                        process = 0;

                    }
                }
            }

            progress.updateValue(25, true);
            progress.setMessage("Recording to AVI");

            int total = data + blank;
            int progressNum = 0;

            for (Enumeration en = transformVector.elements(); en.hasMoreElements();) {

                try {
                    Object obj = en.nextElement();

                    if (obj instanceof java.lang.String) {
                        aviFile.writeBlankFrame();
                    } else {

                        if (((SceneStatePlotter) (obj)).transform != null) {
                            parentScene.getSceneRootTG().setTransform(((SceneStatePlotter) (obj)).transform);
                        }

                        ((SurfacePlotter) parentScene).setSceneState(obj);
                        parentScene.setGUI(obj);

                        captureScreen();
                    }
                } catch (IOException error) {
                    MipavUtil.displayError("Error while trying to capture screen in recorder.");

                    return;
                }

                progressNum++;
                progress.updateValue(25 + (75 * progressNum / total), true);
            }

            progress.dispose();

            if (compression != 0) {
                String fileName = aviFile.getFileName();
                String fileDir = aviFile.getFileDir();

                String fileExt = "";

                File file2delete = new File(fileDir + fileName);

                if (!file2delete.exists()) {
                    System.err.println("File does not exist: cancelling transcoding");

                    return;
                }

                if (compression == 2) {
                    fileExt = ".mov";
                } else if (compression == 3) {
                    fileExt = "_MJPEG.avi";
                } else {
                    fileExt = "_MP42.avi";
                }

                AlgorithmTranscode at = null;

                try {
                    at = new AlgorithmTranscode(file2delete.toURI().toURL(),
                                                fileDir + fileName.substring(0, fileName.length() - 4) + fileExt,
                                                compression);
                    at.setRunningInSeparateThread(true);
                    at.setQuality(mjpegQuality);
                } catch (Exception ex) {
                    System.err.println("MalformedURLException in MouseRecorder transcoding");

                    return;
                }

                at.run();

                // delete the uncompressed file
                FileDeleter fd = new FileDeleter(file2delete.getPath());

                fd.start();
            }

            aviFile.close();
            System.gc();
        }

        /**
         * Captures the canvas. Saves it as an RGB image and sends it to the file avi writer.
         *
         * @throws  IOException  DOCUMENT ME!
         */
        private void captureScreen() throws IOException {
            raster.setImage(javaImage);
            parentScene.getCanvas().getGraphicsContext3D().readRaster(raster);

            // get the RGB array from the image currently on the canvas
            // Image imagePix = raster.getImage().getImage().getScaledInstance(xScale, yScale, Image.SCALE_REPLICATE);
            imagePix = raster.getImage().getImage().getScaledInstance(xScale, yScale, Image.SCALE_REPLICATE);

            PixelGrabber pgTest = new PixelGrabber(imagePix, 0, 0, xScale, yScale, pixels, 0, xScale);

            try {
                pgTest.grabPixels();
            } catch (InterruptedException error) { }

            int index = 0;

            // turn the int RGB into a short, separating the alpha, red, green, and blue values.
            // now will be in MIPAV format
            for (int k = 0; k < pixels.length; k++) {
                buffer[index++] = (short) (255); // alpha
                buffer[index++] = (short) ((pixels[k] >> 16) & 0xFF); // Red
                buffer[index++] = (short) ((pixels[k] >> 8) & 0xFF); // Green
                buffer[index++] = (short) (pixels[k] & 0xFF); // Blue
            }

            // create new model image
            // put short RGB buffer into image
            image.importData(0, buffer, true);

            // if we haven't written the header yet
            if (!set) {

                // write the header, then the data
                aviFile.setAVIWrite(image, interval * 1000, data, blank);
                set = true;
            } else {

                // otherwise write the data
                aviFile.writeDataFrame(image);
            }
        }
    }

}
