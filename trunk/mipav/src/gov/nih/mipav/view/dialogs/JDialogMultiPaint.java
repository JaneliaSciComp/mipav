package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * .
 *
 * <p>Dialog box for the advanced paint power tools: morphology operations, object delete, etc. Bring up this dialog
 * from the normal power paint dialog.</p>
 *
 * @version  May 2005
 * @author   Pierre-Louis Bazin
 * @see      JDialogBase
 * @see      AlgorithmInterface
 */
public class JDialogMultiPaint extends JDialogBase implements MouseListener, KeyListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7497259210728078264L;

    /** DOCUMENT ME! */
    private static ModelLUT lutB;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int alphaBlend = 50;
    
    /** DOCUMENT ME! */
    private int triPlanarAlphaBlend = 50;

    /** DOCUMENT ME! */
    private JPanel bottomPanel;

    /** DOCUMENT ME! */
    private JToggleButton buttonShortkeys;

    /** this is the array list of texts for the mask number buttons. */
    private ArrayList buttonTextArrayList;

    /** Ref to JDialogChangeMaskNumber.* */
    private JDialogChangeMaskNumber changeMaskNumberDialog;

    /** Button that allows user to collapse masks/paint to single value.* */
    private JButton collapseButton;

    /** array of colors to use for the labels. */
    private Color[] color;

    /** private String title; private boolean useTriplanar=false;. */
    private BitSet currentMask;
    
    /** masks in tr-planar **/
    private BitSet[] triPlanarCurrentMasks;

    /** display masks toggle button. */
    private JToggleButton displayMasksButton;

    /** DOCUMENT ME! */
    private JToggleButton displayModeButton;

    /** DOCUMENT ME! */
    private JToggleButton displayPaintButton;

    /** DOCUMENT ME! */
    private JPanel displayPanel;

    /** DOCUMENT ME! */
    private JCheckBox editBox;

    /** button VOI export. */
    private JButton exportVoiButton;

    /** DOCUMENT ME! */
    private JPanel filePanel;

    /** source image. */
    private ModelImage image;

    /** The size, in voxels, of the mask. */
    private int imgBSize;

    /** button VOI import. */
    private JButton importVoiButton;

    /** DOCUMENT ME! */
    private JProgressBar indeterminateProgressBar = new JProgressBar();

    /** DOCUMENT ME! */
    private Vector intensityLockVector = new Vector();

    /** labels for the painted objects. */
    private String[] label;

    /** DOCUMENT ME! */
    private JTextField[] labelField;

    /** DOCUMENT ME! */
    private BorderedButton[] listButton;

    /** DOCUMENT ME! */
    private JPanel listPanel;

    /** DOCUMENT ME! */
    private JFileChooser loadDialog;

    /** DOCUMENT ME! */
    private JButton loadLabelsButton;

    /** DOCUMENT ME! */
    private JButton loadMaskButton;

    /** dialog elements. */
    private JPanel mainPanel;

    /** DOCUMENT ME! */
    private JPanel maskPanel;

    /** DOCUMENT ME! */
    private JPanel miscPanel;

    /** DOCUMENT ME! */
    private BorderedButton[] multiButton;

    /** DOCUMENT ME! */
    private JPanel multiPanel;

    /** number of paint masks initially. */
    private int nbx = 4, nby = 6;

    /** This represents the button the user selects to..initally at 1.* */
    private int newSelection = 1;

    /**
     * private boolean displayMask,displayPaint; // check whether the mask and paint are displayed private boolean
     * editable; // wether or not you can edit the object names.
     */
    // private Color nullColor = Color.gray;

    /** DOCUMENT ME! */
    private JLabel numberLabel;

    /** DOCUMENT ME! */
    private JPanel numberPanel;

    /** resize x value. */
    private JTextField numberXField;

    /** resize y value. */
    private JTextField numberYField;

    /** DOCUMENT ME! */
    private JPanel optionPanel;

    /** DOCUMENT ME! */
    private JCheckBox[] preserveBox;

    /** check whether the mask can be affected by new paint or not. */
    private boolean[] preserved;

    /** resize button. */
    private JButton resizeButton;

    /** DOCUMENT ME! */
    private JFileChooser saveDialog;

    /** save labels button. */
    private JButton saveLabelsButton;

    /** save mask button. */
    private JButton saveMaskButton;

    /** This is the scroll pane for the label list panel.* */
    private JScrollPane scrollPane;

    /** id for the selected paint mask. */
    private int selected = 1;

    /** private int destExtents[];. */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JPanel voiPanel, leftPanel, rightPanel, leftRightPanel;


    /** The mask size in the x dimension. */
    private int xDim;

    /** The mask size in the y dimension. */
    private int yDim;

    /** The mask size in the z dimension. */
    private int zDim;
    
    /** lock all masks checkbox **/
    private JButton lockAllButton;
    
    /** unlock all masks checkbox **/
    private JButton unlockAllButton;
    
    /** lock panel */
    private JPanel lockPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  im  Source image.
     */
    public JDialogMultiPaint(ModelImage im) {
        super();
        userInterface = ViewUserInterface.getReference();
        image = im;
    }

    /**
     * Creates dialog for plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMultiPaint(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        userInterface = ViewUserInterface.getReference();
        image = im;

        // first lets refresh the image
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        // ModelImage imgB = image.getParentFrame().getImageB();
        refreshImagePaint(image, obj);

        init();

        if (theParentFrame instanceof ViewJFrameImage) {
            ViewJFrameImage vjfi = (ViewJFrameImage) theParentFrame;

            if ((vjfi != null) && (vjfi.getLUTb() != null)) {

                if (vjfi.getLUTb().getLUTType() != ModelLUT.STRIPED) {
                    MipavUtil.displayInfo("This tool works best when image B has a striped LUT.");
                }
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Processes the events from this dialog's buttons.
     *
     * @param  event  Event that triggers the action.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Close")) {

            // we need to commit the paint to mask
            BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();
            ModelImage imgB = image.getParentFrame().getImageB();
            image.getParentFrame().getComponentImage().setIntensityDropper((float) (new Integer(multiButton[selected].getText()).intValue()));
            image.getParentFrame().getComponentImage().commitMask(imgB, true, true, intensityLockVector, false);
            image.getParentFrame().getComponentImage().setModifyFlag(true);
            image.notifyImageDisplayListeners();
            refreshImagePaint(image, obj);

            if (changeMaskNumberDialog != null) {
                changeMaskNumberDialog.dispose();
            }

            image.getParentFrame().getControls().getTools().setPointerSelected();
            image.getParentFrame().getComponentImage().setCursorMode(ViewJComponentEditImage.DEFAULT);
            removeKeyListener(this);
            setFocusable(false);
            image.getParentFrame().getComponentImage().removeKeyListener(this);
            image.getParentFrame().getComponentImage().setFocusable(false);
            dispose();
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("10027");
        } else if (command.startsWith("PaintMask")) {

            int num = Integer.valueOf(command.substring(command.indexOf(" ") + 1)).intValue();
            newSelection = num;
            int colorNum = 1;
            //the text on the buttons represent the color
            for(int i=0;i<multiButton.length;i++) {
            	if(multiButton[i] != null) {
            		if(event.getSource() == multiButton[i]) {
            			colorNum = Integer.parseInt(multiButton[i].getText());
            		}
            	}
            }
            for(int i=0;i<listButton.length;i++) {
            	if(listButton[i] != null) {
            		if(event.getSource() == listButton[i]) {
            			colorNum = Integer.parseInt(listButton[i].getText());
            		}
            	}
            }
            // convert the paint to previous selection
            // and the newly selected mask to paint
            switchPaintAndMask(selected, num, colorNum);
        } else if (command.startsWith("Preserve")) {
            int num = Integer.valueOf(command.substring(command.indexOf(" ") + 1)).intValue();

            if (preserveBox[num].isSelected()) {
                preserved[num] = true;

                if ((parentFrame != null) && (parentFrame instanceof ViewJFrameImage)) {
                    addIntensityLock(Integer.parseInt(listButton[num].getText()));
                }
            } else {
                preserved[num] = false;

                if ((parentFrame != null) && (parentFrame instanceof ViewJFrameImage)) {
                    removeIntensityLock(Integer.parseInt(listButton[num].getText()));
                }
            }
        } else if (command.startsWith("Label")) {
            int num = Integer.valueOf(command.substring(command.indexOf(" ") + 1)).intValue();

            label[num] = labelField[num].getText();
            multiButton[num].setToolTipText(label[num]);
        } else if (command.equals("SwitchMode")) {

            if (displayModeButton.isSelected()) {
                multiPanel.setVisible(false);
                scrollPane.setVisible(true);
                lockPanel.setVisible(true);
            } else {

                // refresh the text tooltips
                for (int n = 1; n < ((nbx * nby) + 1); n++) {
                    label[n] = labelField[n].getText();
                    multiButton[n].setToolTipText(label[n]);
                }

                scrollPane.setVisible(false);
                lockPanel.setVisible(false);
                multiPanel.setVisible(true);
            }

            pack();
            repaint();
        } else if (command.equals("Editable")) {

            if (editBox.isSelected()) {

                for (int n = 1; n < ((nbx * nby) + 1); n++) {
                    labelField[n].setEditable(true);
                }
            } else {

                for (int n = 1; n < ((nbx * nby) + 1); n++) {
                    labelField[n].setEditable(false);
                }
            }
        } else if (command.equals("Resize")) {
            int Nbx = Integer.valueOf(numberXField.getText()).intValue();
            int Nby = Integer.valueOf(numberYField.getText()).intValue();
            if(Nbx * Nby != nbx * nby) {
	            // we need to commit paint to mask
	            BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();
	            ModelImage imgB = image.getParentFrame().getImageB();
	            image.getParentFrame().getComponentImage().setIntensityDropper((float) (new Integer(multiButton[selected].getText()).intValue()));
	            image.getParentFrame().getComponentImage().commitMask(imgB, true, true, intensityLockVector, false);
	            image.getParentFrame().getComponentImage().setModifyFlag(true);
	            image.notifyImageDisplayListeners();
	            intensityLockVector = null;
	            refreshImagePaint(image, obj);
	
	            if (getMaskTreeSet(imgB).size() > (Nbx * Nby)) {
	                MipavUtil.displayError("Number of masks exceed resize number");
	                selectedMaskToPaint(selected);
	            } else {
	                refreshImagePaint(image, obj);
	                newLabelList(Nbx, Nby);
	                refreshLabelDisplay();
	            }
            }

        } else if (command.equals("HidePaint")) {

            if (displayPaintButton.isSelected()) {
            	
                currentMask = (BitSet) image.getParentFrame().getComponentImage().getPaintMask().clone();
                refreshImagePaint(image, new BitSet());
                if(image.getTriImageFrame() != null) {
                	triPlanarCurrentMasks = new BitSet[image.getTriImageFrame().MAX_TRI_IMAGES];
                	for (int i = 0; i < image.getTriImageFrame().MAX_TRI_IMAGES; i++) {
                        if (image.getTriImageFrame().triImage[i] != null) {
                        	triPlanarCurrentMasks[i] = (BitSet) image.getTriImageFrame().getTriImage(i).getPaintMask().clone();
                        }
                    }
                } 
            } else {
                refreshImagePaint(image, currentMask);
            }
        } else if (command.equals("HideMasks")) {

            if (displayMasksButton.isSelected()) {
                alphaBlend = (int) (image.getParentFrame().getComponentImage().getAlphaBlend() * 100.0f);
                image.getParentFrame().getComponentImage().setAlphaBlend(100);
                if(image.getTriImageFrame() != null) {
                	triPlanarAlphaBlend = (int)(image.getTriImageFrame().getAlphaBlend() * 100.0f);
                	image.getTriImageFrame().setAlphaBlend(100);
                }   
            } else {
                image.getParentFrame().getComponentImage().setAlphaBlend(alphaBlend);
                if(image.getTriImageFrame() != null) {
                	image.getTriImageFrame().setAlphaBlend(triPlanarAlphaBlend);
                }
            }

            refreshImagePaint(image, image.getParentFrame().getComponentImage().getPaintMask());
        } else if (command.equals("Save")) {
            buildSaveDialog();
            saveDialog.setSize(500, 326);
        } else if (command.equals("Load")) {
            buildLoadDialog();
            loadDialog.setSize(500, 326);
        } else if (command.equals("SaveMask")) {
            int num = selected;
            commitPaintToMask(num);
            image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "SaveMask"));
            selectedMaskToPaint(num);
        } else if (command.equals("LoadMask")) {
            deselectMask();
            image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "OpenMask"));
            imageBInit();
            // selectedMaskToPaint(1);
        } else if (command.equals("ImportVOI")) {
            deselectMask();

            /** I changed this to onlyActive is false... you can switch*/
            image.getParentFrame().setImageB(image.generateUnsignedByteImage(1, false, false));
            image.getParentFrame().getLUTb().makeStripedLUT();

            // import the VOI labels as well
            VOIVector voi = image.getVOIs();
            int Nlabel = voi.size();

            for (int n = 0; n < Nlabel; n++) {
                label[n + 1] = voi.VOIAt(n).getName();
            }

            selectedMaskToPaint(1);
        } else if (command.equals("ExportVOI")) {
            int num = selected;
            commitPaintToMask(num);

            // empty VOI set to start
            image.getParentFrame().getImageB().resetVOIs();

            AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(image.getParentFrame().getImageB());
            progressBar = new ViewJProgressBar(image.getParentFrame().getImageB().getImageName(), "Extracting VOI ...",
                                               0, 100, true);
            progressBar.setSeparateThread(false);
            VOIExtractionAlgo.addProgressChangeListener(progressBar);
            VOIExtractionAlgo.setProgressValues(0, 100);


            // in case there are labels not accounted for
            int Nlabel = countMaskLabels();
            String[] correctedLabels = new String[Nlabel];

            for (int i = 0; i < Nlabel; i++) {

                if (i < (label.length - 1)) {
                    correctedLabels[i] = new String(label[i + 1]);
                } else {
                    correctedLabels[i] = ("VOI" + i);
                }
            }

            VOIExtractionAlgo.setNameTable(correctedLabels);
            VOIExtractionAlgo.run();

            // transfer the VOIs to the original image (and keep in the mask)
            VOIVector voi = image.getParentFrame().getImageB().getVOIs();
            image.resetVOIs();
            image.setVOIs(voi);
            deselectMask();
            selectedMaskToPaint(num);
        } else if (command.equals("Collapse")) {

            // go through image b and find all mask values
            ModelImage imageB = image.getParentFrame().getImageB();
            TreeSet vals = getMaskTreeSet(imageB);

            image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST,
                                                                   "CollapseAllToSinglePaint"));

            BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();
            ModelImage imgB = image.getParentFrame().getImageB();
            image.getParentFrame().getComponentImage().setIntensityDropper((float) (new Integer(multiButton[selected].getText()).intValue()));
            image.getParentFrame().getComponentImage().commitMask(imgB, true, true, intensityLockVector, false);
            image.getParentFrame().getComponentImage().setModifyFlag(true);
            image.notifyImageDisplayListeners();
            refreshImagePaint(image, obj);
        } else if (command.equals("Shortkeys")) {

            if (buttonShortkeys.isSelected()) {
                setFocusable(true);
                addKeyListener(this);
                requestFocusInWindow();
                image.getParentFrame().getComponentImage().setFocusable(true);
                image.getParentFrame().getComponentImage().addKeyListener(this);
                image.getParentFrame().getComponentImage().requestFocusInWindow();
            } else {
                removeKeyListener(this);
                setFocusable(false);
                image.getParentFrame().getComponentImage().removeKeyListener(this);
                image.getParentFrame().getComponentImage().setFocusable(false);
            }
        } else if(command.equals("lockAll")) {
        	lockAll();
        } else if(command.equals("unlockAll")) {
        	unlockAll();
        }

    }

    /**
     * Adds an Integer object to the intensityLockVector. The Integer object represents an intensity value which is
     * locked - that is, cannot be overwritten by a "Paint to mask" operation.
     *
     * @param  intensity  the intensity value to lock
     */
    public void addIntensityLock(int intensity) {
        if (intensityLockVector == null) {
            intensityLockVector = new Vector();
        }

        // is this intensity value already in the 'intensityLockVector' Vector?
        for (int i = 0; i < intensityLockVector.size(); i++) {

            try {
                Integer lockedIntensity = (Integer) intensityLockVector.elementAt(i);

                if ((lockedIntensity != null) && (lockedIntensity.intValue() == intensity)) {

                    // prevent locking an intensity that is already locked
                    return;
                }
            } catch (Exception e) {
                continue;
            }
        }

        Integer intensityLockInteger = new Integer(intensity);

        intensityLockVector.add(intensityLockInteger);
    }

    /**
     * Procedure that counts the number of labels in the mask image.
     *
     * @return  DOCUMENT ME!
     */
    public int countMaskLabels() {
        float[] buffer;
        int Nlabel = 0;
        int MAX_LABEL = 256;

        try {
            int length = 1;

            for (int n = 0; n < image.getExtents().length; n++) {
                length *= image.getExtents()[n];
            }

            buffer = new float[length];
            image.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            MipavUtil.displayError("source image locked");

            return 0;
        } catch (OutOfMemoryError e) {
            buffer = null;
            MipavUtil.displayError("out of memory");

            return 0;
        }

        float[] labelList = new float[MAX_LABEL];
        Nlabel = 0;

        for (int n = 0; (n < buffer.length) && (Nlabel < MAX_LABEL); n++) {

            if (buffer[n] != 0) {
                boolean isNew = true;

                for (int l = 0; l < Nlabel; l++) {

                    if (labelList[l] == buffer[n]) {
                        isNew = false;
                    }
                }

                if (isNew) {
                    labelList[Nlabel] = buffer[n];
                    Nlabel++;
                }
            }
        }

        // userInterface.setMessageText("labels: "+Nlabel);
        if (Nlabel > MAX_LABEL) {
            buffer = null;
            labelList = null;
            MipavUtil.displayError("too many labels");

            return 0;
        }

        return Nlabel;
    }


    /**
     * DOCUMENT ME!
     */
    public void imageBInit() {

        // getting image b size
        ModelImage imgB = image.getParentFrame().getImageB();


        TreeSet vals = getMaskTreeSet(imgB);


        // iterate over TreeSet and set color[] and backgrounds
        Iterator iter = vals.iterator();
        boolean match = false;
        int k = 1;

        // this boolean is needed to test if image b has a color of 1 in it
        boolean hasOne = false;
        if (vals.size() == 0) {
            color[1] = lutB.getColor(1);
            multiButton[1].setBackground(color[1]);
            listButton[1].setBackground(color[1]);
            multiButton[1].setText(String.valueOf(1));
            listButton[1].setText(String.valueOf(1));
            buttonTextArrayList.set(0, new Integer(1));
            multiButton[1].setSelected(true);
            listButton[1].setSelected(true);
            image.getParentFrame().getComponentImage().setIntensityDropper((float) (new Integer(multiButton[1].getText()).intValue()));
            image.getParentFrame().getControls().getTools().setPaintColor(color[1]);

            image.getParentFrame().getComponentImage().updatePaintBrushCursor();
            
            if (image.getTriImageFrame() != null) {
                image.getTriImageFrame().setIntensityDropper((float) (new Integer(multiButton[1].getText()).intValue()));
                image.getTriImageFrame().setPaintColor(color[1]);

            }

            ((ViewJFrameImage) image.getParentFrame()).handleMaskToPaint(false);
        }

        while (iter.hasNext()) {
            Integer maskVal = (Integer) iter.next();
            int maskVal_int = maskVal.intValue();
          
            if (maskVal_int == 1) {
                hasOne = true;
            }

            for (k = 1; k <= buttonTextArrayList.size(); k++) {

                if (maskVal_int == ((Integer) buttonTextArrayList.get(k - 1)).intValue()) {
                    match = true;

                    break;
                }
            }

            // since treeset is ordered, this will work
            if (match) {
                color[k] = lutB.getColor(maskVal_int);
                multiButton[k].setBackground(color[k]);
                listButton[k].setBackground(color[k]);
                multiButton[k].setText(String.valueOf(maskVal_int));
                listButton[k].setText(String.valueOf(maskVal_int));
                buttonTextArrayList.set(k - 1, new Integer(maskVal_int));
            } else {

                for (int n = 1; n <= (nbx * nby); n++) {

                    if (!hasOne) {
                        if (color[1].getRGB() == lutB.getColor(1).getRGB()) {
                            color[1] = lutB.getColor(maskVal_int);
                            multiButton[1].setBackground(color[1]);
                            listButton[1].setBackground(color[1]);
                            multiButton[1].setText(String.valueOf(maskVal_int));
                            listButton[1].setText(String.valueOf(maskVal_int));
                            buttonTextArrayList.set(0, new Integer(maskVal_int));

                            break;
                        }
                    }

                    if (color[n] == null) {
                        color[n] = lutB.getColor(maskVal_int);
                        multiButton[n].setBackground(color[n]);
                        listButton[n].setBackground(color[n]);
                        multiButton[n].setText(String.valueOf(maskVal_int));
                        listButton[n].setText(String.valueOf(maskVal_int));
                        buttonTextArrayList.set(n - 1, new Integer(maskVal_int));

                        break;
                    }
                }


            }

            match = false;
        }

        // since the image b is not blank, we need to figure out which button should be selected
        boolean test = false;

        if (vals.size() > 0) {
            int n;


            if (vals.size() >= (nbx * nby)) {
                selected = 1;

                // color[n] = lutB.getColor(n);
                multiButton[1].setBackground(color[1]);
                multiButton[1].setSelected(true);

                // multiButton[1].setSelected(false);
                listButton[1].setBackground(color[1]);
                listButton[1].setSelected(true);

                // listButton[1].setSelected(false);
                image.getParentFrame().getComponentImage().setIntensityDropper((float) (new Integer(multiButton[1].getText()).intValue()));
                image.getParentFrame().getControls().getTools().setPaintColor(color[1]);

                image.getParentFrame().getComponentImage().updatePaintBrushCursor();
                
                if (image.getTriImageFrame() != null) {
                    image.getTriImageFrame().setIntensityDropper((float) (new Integer(multiButton[1].getText()).intValue()));
                    image.getTriImageFrame().setPaintColor(color[1]);

                }

                ((ViewJFrameImage) image.getParentFrame()).handleMaskToPaint(false);
            } else {

                for (n = 1; n <= (nbx * nby); n++) {

                    if (!hasOne) {
                        if (color[1].getRGB() == lutB.getColor(1).getRGB()) {
                            selected = 1;
                            multiButton[1].setBackground(color[1]);
                            multiButton[1].setSelected(true);
                            listButton[1].setBackground(color[1]);
                            listButton[1].setSelected(true);
                            image.getParentFrame().getComponentImage().setIntensityDropper((float)
                                                                                           (new Integer(multiButton[1].getText()).intValue()));
                            image.getParentFrame().getControls().getTools().setPaintColor(color[1]);

                            image.getParentFrame().getComponentImage().updatePaintBrushCursor();
                            
                            if (image.getTriImageFrame() != null) {
                                image.getTriImageFrame().setIntensityDropper((float) (new Integer(multiButton[1].getText()).intValue()));
                                image.getTriImageFrame().setPaintColor(color[1]);

                            }

                            test = true;

                            break;
                        }
                    }

                    if (color[n] == null) {
                        selected = n;
                        color[n] = lutB.getColor(n);
                        multiButton[n].setBackground(color[n]);
                        multiButton[n].setSelected(true);
                        multiButton[1].setSelected(false);
                        listButton[n].setBackground(color[n]);
                        listButton[n].setSelected(true);
                        listButton[1].setSelected(false);
                        image.getParentFrame().getComponentImage().setIntensityDropper((float)
                                                                                       (new Integer(multiButton[n].getText()).intValue()));
                        image.getParentFrame().getControls().getTools().setPaintColor(color[n]);

                        image.getParentFrame().getComponentImage().updatePaintBrushCursor();
                        
                        if (image.getTriImageFrame() != null) {
                            image.getTriImageFrame().setIntensityDropper((float) (new Integer(multiButton[n].getText()).intValue()));
                            image.getTriImageFrame().setPaintColor(color[n]);

                        }

                        test = true;

                        break;
                    }
                }
            }


        }
    }

    /**
     * Handle the key pressed event.
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyPressed(KeyEvent e) { }

    /**
     * Handle the key released event.
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyReleased(KeyEvent e) { }

    /**
     * Handle the key typed event.
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyTyped(KeyEvent e) {
        String key = Character.toString(e.getKeyChar());


        if (key.equals("1")) {

            actionPerformed(new ActionEvent(this, 0, "PaintMask 1"));
        } else if (key.equals("2")) {
            actionPerformed(new ActionEvent(this, 0, "PaintMask 2"));
        } else if (key.equals("3")) {
            actionPerformed(new ActionEvent(this, 0, "PaintMask 3"));
        } else if (key.equals("4")) {
            actionPerformed(new ActionEvent(this, 0, "PaintMask 4"));
        } else if (key.equals("5")) {
            actionPerformed(new ActionEvent(this, 0, "PaintMask 5"));
        } else if (key.equals("6")) {
            actionPerformed(new ActionEvent(this, 0, "PaintMask 6"));
        } else if (key.equals("7")) {
            actionPerformed(new ActionEvent(this, 0, "PaintMask 7"));
        } else if (key.equals("8")) {
            actionPerformed(new ActionEvent(this, 0, "PaintMask 8"));
        } else if (key.equals("9")) {
            actionPerformed(new ActionEvent(this, 0, "PaintMask 9"));
        } else if (key.equals("t")) {
            displayModeButton.setSelected(!displayModeButton.isSelected());
            actionPerformed(new ActionEvent(this, 0, "SwitchMode"));
        } else if (key.equals("c")) {
            displayPaintButton.setSelected(!displayPaintButton.isSelected());
            actionPerformed(new ActionEvent(this, 0, "HidePaint"));
        } else if (key.equals("v")) {
            displayMasksButton.setSelected(!displayMasksButton.isSelected());
            actionPerformed(new ActionEvent(this, 0, "HideMasks"));
        } else if (key.equals("s")) {
            actionPerformed(new ActionEvent(this, 0, "Save"));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent e) {

        if (e.getButton() == MouseEvent.BUTTON3) {

            if (e.getSource() instanceof JButton) {
                JPanel correspondingPanel = null;
                String correspondingButtonName = "";
                JButton correspondingButton = null;
                JPanel muPanel = null;
                JPanel liPanel = null;
                JScrollPane scPane = null;

                // get Source Button and Source Panel
                JButton sourceButton = (JButton) e.getSource();
                JPanel srcPanel = (JPanel) sourceButton.getParent();
                String srcPanelName = srcPanel.getName();


                // get ContentPane
                Container contentPane = this.getContentPane();

                // get Main Panel
                JPanel mPanel = (JPanel) contentPane.getComponents()[0];

                // get Correponding Panel
                for (int k = 0; k < mPanel.getComponentCount(); k++) {

                    if (mPanel.getComponents()[k].getName() != null) {

                        if (mPanel.getComponents()[k].getName().equals("multiPanel")) {
                            muPanel = (JPanel) mPanel.getComponents()[k];
                        } else if (mPanel.getComponents()[k].getName().equals("scrollPane")) {
                            scPane = (JScrollPane) mPanel.getComponents()[k];
                        }
                    }
                }

                if (srcPanelName.equals("multiPanel")) {
                    JViewport vPort = scPane.getViewport();

                    for (int k = 0; k < vPort.getComponentCount(); k++) {

                        if (vPort.getComponents()[k].getName().equals("listPanel")) {
                            liPanel = (JPanel) vPort.getComponents()[k];
                        }
                    }

                    correspondingPanel = liPanel;
                } else if (srcPanelName.equals("listPanel")) {
                    correspondingPanel = muPanel;
                }

                // get Corresponding Button Name
                String sourceButtonName = sourceButton.getName();

                if (sourceButtonName.split(":")[0].equals("multiButton")) {
                    correspondingButtonName = "listButton:" + sourceButtonName.split(":")[1];
                } else {
                    correspondingButtonName = "multiButton:" + sourceButtonName.split(":")[1];
                }

                // get Corresponding Button
                for (int i = 0; i < correspondingPanel.getComponentCount(); i++) {

                    if (correspondingPanel.getComponents()[i].getName() != null) {

                        if (correspondingPanel.getComponents()[i].getName().equals(correspondingButtonName)) {
                            correspondingButton = (JButton) correspondingPanel.getComponents()[i];
                        }
                    }
                }


                int sourceId = (new Integer(sourceButtonName.split(":")[1])).intValue();

                // only allowing this dialog to pop up if the button is actually selected
                if (sourceId == selected) {
                    changeMaskNumberDialog = new JDialogChangeMaskNumber(sourceButton, correspondingButton,
                                                                         buttonTextArrayList, color, lutB, image,
                                                                         newSelection, intensityLockVector);
                    changeMaskNumberDialog.pack();
                    MipavUtil.centerInComponent(this, changeMaskNumberDialog);
                    changeMaskNumberDialog.setVisible();
                }
            }
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent e) {
        // TODO Auto-generated method stub

    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent e) {
        // TODO Auto-generated method stub

    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void mousePressed(MouseEvent e) {
        // TODO Auto-generated method stub

    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void mouseReleased(MouseEvent e) {
        // TODO Auto-generated method stub

    }

    /**
     * Reads the 'labels' file from disk.
     *
     * @param  filename  DOCUMENT ME!
     */
    public void readLabelsFromFile(String filename) {

        try {
            File f = new File(filename);
            FileReader fr = new FileReader(f);
            BufferedReader br = new BufferedReader(fr);
            String line = br.readLine();
            int num;

            // Exact corresponding template
            if (!line.equals("Labels for MultiPaint (edit with care)")) {
                Preferences.debug("not a proper label file");
                br.close();
                fr.close();

                return;
            }

            line = br.readLine();

            // numbers
            if (line.startsWith("Number of labels: ")) {
                int Nbx = Integer.valueOf(line.substring(line.indexOf(":") + 2, line.indexOf("x") - 1)).intValue();
                int Nby = Integer.valueOf(line.substring(line.indexOf("x") + 2)).intValue();
                numberXField.setText("" + Nbx);
                numberYField.setText("" + Nby);

                if (selected > (Nbx * Nby)) {
                    commitPaintToMask(selected);
                }

                newLabelList(Nbx, Nby);

                line = br.readLine();

                for (int n = 1; n < ((nbx * nby) + 1); n++) {
                    line = br.readLine();

                    /*
                     * num = Integer.valueOf(line.substring(0, line.indexOf("|") - 1)).intValue();
                     *
                     * if (num == n) { label[n] = line.substring(line.indexOf("|") + 2, line.lastIndexOf("|") - 1);
                     * color[n] = new Color(Integer.valueOf(line.substring(line.lastIndexOf("|") + 2)).intValue()); }
                     */
                    String[] maskLabelInfo = line.split(":");

                    if (maskLabelInfo.length == 4) {
                        num = new Integer(maskLabelInfo[0].trim()).intValue();
                    } else {
                        num = Integer.valueOf(line.substring(0, line.indexOf("|") - 1)).intValue();
                    }

                    if (num == n) {

                        if (maskLabelInfo.length == 4) {
                            buttonTextArrayList.set(n - 1, new Integer(maskLabelInfo[1].trim()));
                            label[n] = maskLabelInfo[2].trim();

                            if (maskLabelInfo[3].trim().equals("null")) {
                                color[n] = null;
                            } else {
                                color[n] = new Color(new Integer(maskLabelInfo[3].trim()).intValue());
                            }
                        }
                        // need to handle the old labels files also since i added another column for button text
                        else {
                            buttonTextArrayList.set(n - 1, new Integer(n));
                            label[n] = line.substring(line.indexOf("|") + 2, line.lastIndexOf("|") - 1);

                            // since the old files were saving the grey value of the button, we need to check b/c we
                            // dont want the buttons grey
                            if (Integer.valueOf(line.substring(line.lastIndexOf("|") + 2)).intValue() == -8355712) {
                                color[n] = null;
                            } else {
                                color[n] = new Color(Integer.valueOf(line.substring(line.lastIndexOf("|") + 2)).intValue());
                            }
                        }
                    }
                }

                refreshLabelDisplay();
            }

            br.close();
            fr.close();
        } catch (IOException ioe) {
            Preferences.debug(ioe.getMessage());
        }
    }

    /**
     * Removes an intensity value from the intensityLockVector.
     *
     * @param  intensity  the intensity value to remove
     */
    public void removeIntensityLock(int intensity) {

        if (intensityLockVector == null) {
            return;
        }

        for (int i = 0; i < intensityLockVector.size(); i++) {

            try {
                Integer lockedIntensity = (Integer) intensityLockVector.elementAt(i);

                if ((lockedIntensity != null) && (lockedIntensity.intValue() == intensity)) {
                    intensityLockVector.removeElementAt(i);

                    return;
                }
            } catch (Exception e) {
                continue;
            }
        }
    }


    /**
     * windowclosing...override of WindowListener interface.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {

        // we need to commit the paint to mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();
        ModelImage imgB = image.getParentFrame().getImageB();
        image.getParentFrame().getComponentImage().setIntensityDropper((float) (new Integer(multiButton[selected].getText()).intValue()));
        image.getParentFrame().getComponentImage().commitMask(imgB, true, true, intensityLockVector, false);
        image.getParentFrame().getComponentImage().setModifyFlag(true);
        image.notifyImageDisplayListeners();
        refreshImagePaint(image, obj);

        if (changeMaskNumberDialog != null) {
            changeMaskNumberDialog.dispose();
        }

        image.getParentFrame().getControls().getTools().setPointerSelected();
        image.getParentFrame().getComponentImage().setCursorMode(ViewJComponentEditImage.DEFAULT);
        removeKeyListener(this);
        setFocusable(false);
        image.getParentFrame().getComponentImage().removeKeyListener(this);
        image.getParentFrame().getComponentImage().setFocusable(false);
        dispose();
    }

    /**
     * Instantiates and shows the "Load label file" dialog, which is used to load a text file containing the names of
     * the colored labels.
     */
    private void buildLoadDialog() {
        loadDialog = new javax.swing.JFileChooser();
        loadDialog.setDialogTitle("Load label file");
        loadDialog.setDialogType(JFileChooser.OPEN_DIALOG);
        loadDialog.setMaximumSize(new java.awt.Dimension(500, 326));
        loadDialog.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    loadFileActionPerformed(evt);
                }
            });
        loadDialog.setFileSelectionMode(JFileChooser.FILES_ONLY);
        loadDialog.showOpenDialog(this);
    }

    /**
     * Instantiates and shows the "Save label file" dialog, which is used to save a text file containing the names of
     * the colored labels.
     */
    private void buildSaveDialog() {
        saveDialog = new javax.swing.JFileChooser();
        saveDialog.setDialogTitle("Save label file");
        saveDialog.setDialogType(JFileChooser.SAVE_DIALOG);
        saveDialog.setMaximumSize(new java.awt.Dimension(500, 326));
        saveDialog.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    saveFileActionPerformed(evt);
                }
            });
        saveDialog.setFileSelectionMode(JFileChooser.FILES_ONLY);
        saveDialog.showSaveDialog(this);
    }

    /**
     * Converts the paint to a mask. Creates a new mask image if one does not already exist.
     *
     * @param  num  the index into the color array, which indicates the color of the paint
     */
    private void commitPaintToMask(int num) {
        multiButton[num].setSelected(false);
        listButton[num].setSelected(false);

        if (image == null) {
            System.gc();
            MipavUtil.displayError("Error: image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("Error: paint mask not found");

            return;
        }

        // select the ID: if not new, update the mask
        if (image.getParentFrame().getImageB() == null) {

            // create the mask image
            image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "NewMask"));
        }

        // record selected image; set to image B
        // ModelImage active = image.getParentFrame().getActiveImage();
        image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_B);

        // create new color
        if (color[num] == null) {
            color[num] = lutB.getColor(num);

        }

        multiButton[num].setBackground(color[num]);
        listButton[num].setBackground(color[num]);

        // call the paint to mask program for exiting mask
        image.getParentFrame().getComponentImage().setIntensityDropper((float) (new Integer(multiButton[num].getText()).intValue()));
        image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "CommitPaint"));

        selected = 0;

        refreshImagePaint(image, obj);
    }

    /**
     * Sets buttons to deselected, then calls refreshImagePaint to reset paint as mask.
     */
    private void deselectMask() {

        if (selected > 0) {
            multiButton[selected].setSelected(false);
            listButton[selected].setSelected(false);
        }

        selected = 0;
        refreshImagePaint(image, new BitSet());

    }


    /**
     * Determines the intensities of image B.....populates the treeset and returns it.
     *
     * @param   imgB  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private TreeSet getMaskTreeSet(ModelImage imgB) {
        boolean blankImage = true;
        TreeSet vals = new TreeSet();

        xDim = imgB.getExtents()[0];
        yDim = imgB.getExtents()[1];

        int numDims = imgB.getNDims();

        if (numDims == 2) {
            imgBSize = xDim * yDim;
        } else {
            zDim = imgB.getExtents()[2];
            imgBSize = xDim * yDim * zDim;
        }


        if (!image.isColorImage()) {

            for (int i = 0; i < imgBSize; i++) {

                if (imgB.getUByte(i) != 0) {

                    // there is at least some mask...so set blank image to flase
                    blankImage = false;

                    int val = imgB.getUByte(i);
                    vals.add(new Integer(val));
                }
            }
        } else {
        	boolean match = false;
            for (int i = 0; i < (imgBSize * 4); i = i + 4) {
                short r, g, b;
                int k;
                r = imgB.getUByte(i + 1);
                g = imgB.getUByte(i + 2);
                b = imgB.getUByte(i + 3);

                if ((r != 0) || (g != 0) || (b != 0)) {

                    // there is at least some mask...so set blank image to flase
                    blankImage = false;

                    for (k = 0; k < (lutB.getExtents()[1] * 4); k = k + 4) {
                        if ((lutB.getUByte(k + 1) == r) && (lutB.getUByte(k + 2) == g) && (lutB.getUByte(k + 3) == b)) {
                        	match = true;
                            break;
                        }
                    }
                    if(match) {
                    	
                    	int val = k / 4;
                    	vals.add(new Integer(val));
                    }
                    match = false;
                }
            }
        }

        return vals;
    }

    /**
     * Initializes the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Multiple Paint Tools");


        // multiple masks/paint
        label = new String[(nbx * nby) + 1];
        labelField = new JTextField[(nbx * nby) + 1];
        multiButton = new BorderedButton[(nbx * nby) + 1];
        listButton = new BorderedButton[(nbx * nby) + 1];
        preserved = new boolean[(nbx * nby) + 1];
        preserveBox = new JCheckBox[(nbx * nby) + 1];
        color = new Color[(nbx * nby) + 1];
        buttonTextArrayList = new ArrayList();

        for (int n = 1; n < ((nbx * nby) + 1); n++) {
            label[n] = new String("Label " + n);
            labelField[n] = new JTextField(5);
            labelField[n].setText(label[n]);
            labelField[n].setFont(serif12);
            labelField[n].addActionListener(this);
            labelField[n].setActionCommand("Label " + n);

            multiButton[n] = new BorderedButton(String.valueOf(n));
            multiButton[n].setName("multiButton:" + n);
            multiButton[n].addActionListener(this);
            multiButton[n].addMouseListener(this);
            multiButton[n].setActionCommand("PaintMask " + n);
            multiButton[n].setFont(MipavUtil.font10);
            multiButton[n].setSelected(false);
            multiButton[n].setMaximumSize(new Dimension(48, 20));
            multiButton[n].setPreferredSize(new Dimension(48, 20));
            // multiButton[n].setToolTipText(label[n]);

            listButton[n] = new BorderedButton(String.valueOf(n));
            listButton[n].setName("listButton:" + n);
            listButton[n].addActionListener(this);
            listButton[n].addMouseListener(this);
            listButton[n].setActionCommand("PaintMask " + n);
            listButton[n].setFont(MipavUtil.font10);
            listButton[n].setSelected(false);
            listButton[n].setMaximumSize(new Dimension(50, 18));
            listButton[n].setPreferredSize(new Dimension(50, 18));
            // listButton[n].setBackground(nullColor);

            // color[n] = nullColor;
            color[n] = null;

            preserved[n] = false;
            preserveBox[n] = new JCheckBox(MipavUtil.getIcon("unlocked.gif"));
            preserveBox[n].setSelectedIcon(MipavUtil.getIcon("locked.gif"));
            preserveBox[n].addActionListener(this);
            preserveBox[n].setActionCommand("Preserve " + n);
            preserveBox[n].setToolTipText("Lock the paint mask");

            buttonTextArrayList.add(new Integer(n));
        }
        
        multiButton[selected].setSelected(true);
        listButton[selected].setSelected(true);

        // edit, load and save palettes
        editBox = new JCheckBox("Edit");
        editBox.addActionListener(this);
        editBox.setActionCommand("Editable");
        editBox.setToolTipText("Edit the labels");
        editBox.setSelected(true);

        loadLabelsButton = new JButton("Load labels");
        loadLabelsButton.addActionListener(this);
        loadLabelsButton.setActionCommand("Load");
        loadLabelsButton.setFont(serif12);
        loadLabelsButton.setToolTipText("Load a palette");

        saveLabelsButton = new JButton("Save labels");
        saveLabelsButton.addActionListener(this);
        saveLabelsButton.setActionCommand("Save");
        saveLabelsButton.setFont(serif12);
        saveLabelsButton.setToolTipText("Save current palette");

        loadMaskButton = new JButton("Load masks");
        loadMaskButton.addActionListener(this);
        loadMaskButton.setActionCommand("LoadMask");
        loadMaskButton.setFont(serif12);
        loadMaskButton.setToolTipText("Load a mask");

        saveMaskButton = new JButton("Save masks");
        saveMaskButton.addActionListener(this);
        saveMaskButton.setActionCommand("SaveMask");
        saveMaskButton.setFont(serif12);
        saveMaskButton.setToolTipText("Save current mask");

        importVoiButton = new JButton("Import from VOIs");
        importVoiButton.addActionListener(this);
        importVoiButton.setActionCommand("ImportVOI");
        importVoiButton.setFont(serif12);
        importVoiButton.setToolTipText("Create a mask from the image's VOIs");

        exportVoiButton = new JButton("Export to VOIs");
        exportVoiButton.addActionListener(this);
        exportVoiButton.setActionCommand("ExportVOI");
        exportVoiButton.setFont(serif12);
        exportVoiButton.setToolTipText("Convert the masks into VOIs");

        buttonShortkeys = new JToggleButton("Use hotkeys");
        buttonShortkeys.addActionListener(this);
        buttonShortkeys.setActionCommand("Shortkeys");
        buttonShortkeys.setFont(serif12);
        buttonShortkeys.setToolTipText("<html>" + "&nbsp;<u>Use hotkeys for commands:</u>" + "<br>" +
                                       "&nbsp;<b>1-9:</b> change to mask #1-9 " + "<br>" +
                                       "&nbsp;<b>t:</b> show/hide label text " + "<br>" +
                                       "&nbsp;<b>c:</b> show/hide current paint mask&nbsp;" + "<br>" +
                                       "&nbsp;<b>v:</b> show/hide validated masks " + "<br>" +
                                       "&nbsp;<b>s:</b> save masks" + "</html>");

        // customize the mask palette
        numberLabel = new JLabel("Number of masks: ");
        numberLabel.setForeground(Color.black);
        numberLabel.setFont(serif12);
        numberLabel.setToolTipText("Specify the number of labels (col x row)");

        numberXField = new JTextField(3);
        numberXField.setText(String.valueOf(nbx));
        numberXField.setFont(serif12);

        numberYField = new JTextField(3);
        numberYField.setText(String.valueOf(nby));
        numberYField.setFont(serif12);

        resizeButton = new JButton("Resize");
        resizeButton.addActionListener(this);
        resizeButton.setActionCommand("Resize");
        resizeButton.setFont(serif12);
        resizeButton.setToolTipText("Resize the palette to the appropriate dimensions");

        // display options
        displayModeButton = new JToggleButton("Show label text");
        displayModeButton.addActionListener(this);
        displayModeButton.setActionCommand("SwitchMode");
        displayModeButton.setFont(serif12);
        displayModeButton.setToolTipText("Switch between the compact palette and the detailed list modes");

        displayPaintButton = new JToggleButton("Hide paint");
        displayPaintButton.addActionListener(this);
        displayPaintButton.setActionCommand("HidePaint");
        displayPaintButton.setFont(serif12);
        displayPaintButton.setToolTipText("Hide the paint mask for visualization");

        displayMasksButton = new JToggleButton("Hide masks");
        displayMasksButton.addActionListener(this);
        displayMasksButton.setActionCommand("HideMasks");
        displayMasksButton.setFont(serif12);
        displayMasksButton.setToolTipText("Hide inactive masks for visualization");

        collapseButton = new JButton("Collapse masks/paint");
        collapseButton.addActionListener(this);
        collapseButton.setActionCommand("Collapse");
        collapseButton.setFont(serif12);
        collapseButton.setToolTipText("Collapse masks and paint to single value");


        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 2, 2, 2);

        multiPanel = new JPanel(new GridBagLayout());
        multiPanel.setName("multiPanel");
        multiPanel.setBorder(buildTitledBorder("Paint Mask Palette"));

        gbc.insets = new Insets(0, 0, 0, 0);

        for (int i = 0; i < nbx; i++) {

            for (int j = 0; j < nby; j++) {
                gbc.gridx = i;
                gbc.gridy = j;
                gbc.gridwidth = 1;
                gbc.weightx = 1;
                gbc.fill = GridBagConstraints.HORIZONTAL;
                multiPanel.add(multiButton[i + (nbx * j) + 1], gbc);
            }
        }

        listPanel = new JPanel(new GridBagLayout());
        listPanel.setName("listPanel");
        listPanel.setBorder(buildTitledBorder("Label list"));

        for (int i = 0; i < nbx; i++) {

            for (int j = 0; j < nby; j++) {
                gbc.gridy = i + (nbx * j);
                gbc.gridwidth = 1;
                gbc.gridx = 0;
                gbc.weightx = 0;
                gbc.fill = GridBagConstraints.NONE;
                listPanel.add(listButton[i + (nbx * j) + 1], gbc);
                gbc.gridx = 1;
                gbc.weightx = 1;
                gbc.fill = GridBagConstraints.HORIZONTAL;
                listPanel.add(labelField[i + (nbx * j) + 1], gbc);
                gbc.gridx = 2;
                gbc.weightx = 0;
                gbc.fill = GridBagConstraints.NONE;
                listPanel.add(preserveBox[i + (nbx * j) + 1], gbc);
            }
        }
        
        lockPanel = new JPanel();
        lockAllButton = new JButton("lock all masks");
        lockAllButton.addActionListener(this);
        lockAllButton.setActionCommand("lockAll");
        gbc.gridy = 0;
        gbc.gridx = 0;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.insets = new Insets(5, 2, 5, 2);
        lockPanel.add(lockAllButton,gbc);
        
        unlockAllButton = new JButton("unlock all masks");
        unlockAllButton.addActionListener(this);
        unlockAllButton.setActionCommand("unlockAll");
        gbc.gridy = 0;
        gbc.gridx = 1;
        lockPanel.add(unlockAllButton,gbc);
        lockPanel.setVisible(false);

        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 0, 0, 0);

        scrollPane = new JScrollPane(listPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setPreferredSize(new Dimension(400, 560));
        scrollPane.setName("scrollPane");
        scrollPane.setVisible(false);

        optionPanel = new JPanel(new GridBagLayout());
        optionPanel.setBorder(buildTitledBorder("Options"));

        numberPanel = new JPanel(new GridBagLayout());
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        numberPanel.add(numberLabel, gbc);
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 1;
        numberPanel.add(numberXField, gbc);
        gbc.gridx = 2;
        numberPanel.add(numberYField, gbc);
        gbc.gridx = 3;
        numberPanel.add(resizeButton, gbc);

        leftPanel = new JPanel(new GridBagLayout());
        gbc.gridy = 0;
        gbc.gridx = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        leftPanel.add(loadLabelsButton,gbc);
        gbc.gridy = 1;
        leftPanel.add(loadMaskButton,gbc);
        gbc.gridy = 2;
        leftPanel.add(importVoiButton,gbc);
        gbc.gridy = 3;
        leftPanel.add(displayPaintButton,gbc);
        gbc.gridy = 4;
        leftPanel.add(displayModeButton,gbc);
        
        rightPanel = new JPanel(new GridBagLayout());
        gbc.gridy = 0;
        gbc.gridx = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        rightPanel.add(saveLabelsButton,gbc);
        gbc.gridy = 1;
        rightPanel.add(saveMaskButton,gbc);
        gbc.gridy = 2;
        rightPanel.add(exportVoiButton,gbc);
        gbc.gridy = 3;
        rightPanel.add(displayMasksButton,gbc);
        gbc.gridy = 4;
        rightPanel.add(collapseButton,gbc);
        
        leftRightPanel = new JPanel(new GridBagLayout());
        gbc.gridy = 0;
        gbc.gridx = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        leftRightPanel.add(leftPanel,gbc);
        gbc.gridx = 1;
        leftRightPanel.add(rightPanel,gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        optionPanel.add(numberPanel, gbc);
        gbc.gridy = 1;
        optionPanel.add(leftRightPanel, gbc);
        gbc.gridy = 2;
        optionPanel.add(buttonShortkeys, gbc);

        mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setName("mainPanel");
        mainPanel.setForeground(Color.black);

        gbc.gridx = 0;
        gbc.gridwidth = 1;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        gbc.weighty = 1;
        mainPanel.add(multiPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(optionPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(scrollPane, gbc);
        gbc.gridy = 3;
        mainPanel.add(lockPanel, gbc);

        bottomPanel = new JPanel(new GridBagLayout());
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.insets = new Insets(0, 2, 2, 2);
        bottomPanel.add(indeterminateProgressBar, gbc);
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.NONE;
        gbc.weightx = 1;
        bottomPanel.add(buildCloseButton(), gbc);
        // bottomPanel.add(buildHelpButton());

        getContentPane().add(mainPanel);
        getContentPane().add(bottomPanel, BorderLayout.SOUTH);

        initBlankPaint(1);

        imageBInit();

        pack();
        setVisible(true);
        setResizable(true);
        System.gc();

    } // end init()
    
    
    /**
     * locks all masks
     *
     */
    private void lockAll() {
    	for(int i=1;i<preserveBox.length;i++) {
    		if(!preserveBox[i].isSelected()) {
    			preserveBox[i].setSelected(true);
    			int num = Integer.parseInt(listButton[i].getText());
    			addIntensityLock(Integer.parseInt(listButton[i].getText()));
    		}
    	}
    }
    
    /**
     * unlocks all masks
     *
     */
    private void unlockAll() {
    	for(int i=1;i<preserveBox.length;i++) {
    		if(preserveBox[i].isSelected()) {
    			preserveBox[i].setSelected(false);
    			int num = Integer.parseInt(listButton[i].getText());
    			removeIntensityLock(Integer.parseInt(listButton[i].getText()));
    		}
    	}
    }

    /**
     * Initializes a new blank paint mask to the color indexed by the parameter 'num'
     *
     * @param  num  the index into the color array
     */
    private void initBlankPaint(int num) {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("Error: image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("Error: paint mask not found");

            return;
        }

        // select the ID: if not new, update the mask
        if (image.getParentFrame().getImageB() == null) {

            // create the mask image
            image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "NewMask"));
        }

        // record selected image; set to image B
        ModelImage active = image.getParentFrame().getActiveImage();
        image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_B);

        // set up LUTb
        if (image.isColorImage()) {

            // need to instantiate a LUTb
            lutB = new ModelLUT(ModelLUT.STRIPED, 256, new int[] { 4, 256 });
        } else {
            lutB = image.getParentFrame().getLUTb();
        }

        // create new color
        color[num] = lutB.getColor(num);
        multiButton[num].setBackground(color[num]);
        listButton[num].setBackground(color[num]);


        // call the paint to mask program for exiting mask
        image.getParentFrame().getComponentImage().setIntensityDropper((float) num);
        image.getParentFrame().getControls().getTools().setPaintColor(color[num]);

        image.getParentFrame().getComponentImage().updatePaintBrushCursor();
        
        // tri-image
        if (image.getTriImageFrame() != null) {
            image.getTriImageFrame().setIntensityDropper((float) num);
            image.getTriImageFrame().setPaintColor(color[num]);

        }


        // reset the active image and intensity label
        if (!active.equals(image.getParentFrame().getActiveImage())) {
            image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_A);
        }

        selected = num;
        multiButton[selected].setSelected(true);
        listButton[selected].setSelected(true);

        refreshImagePaint(image, obj);
    }

    /**
     * Handles the action event generated by the "Load label file" dialog. Calls readLabelsFromFile(String) to read the
     * label file selected by the user.
     *
     * @param  evt  the ActionEvent generated by this dialog
     */
    private void loadFileActionPerformed(ActionEvent evt) {

        if (JFileChooser.APPROVE_SELECTION.equals(evt.getActionCommand())) {
            String filename = loadDialog.getSelectedFile().getAbsolutePath();
            userInterface.setGlobalDataText("data file: " + filename + "\n");
            readLabelsFromFile(filename);
            userInterface.setGlobalDataText("label data loaded from " + filename + "\n");
        }
        // loadDialog.setVisible(false);
    }

    /**
     * Purpose: unknown.
     *
     * @param  Nbx  number of labels in the x-direction
     * @param  Nby  number of labels in the y-direction
     */
    private void newLabelList(int Nbx, int Nby) {
        String[] newlabel = new String[(Nbx * Nby) + 1];
        boolean[] newpreserved = new boolean[(Nbx * Nby) + 1];
        Color[] newcolor = new Color[(Nbx * Nby) + 1];


        if ((Nbx * Nby) < (nbx * nby)) {
            ArrayList newArr = new ArrayList();

            for (int k = 0; k < ((Nbx * Nby) + 1); k++) {

                Integer i = (Integer) buttonTextArrayList.get(k);
                newArr.add(i);
            }

            buttonTextArrayList = newArr;
        }

        for (int n = 1; n < ((Nbx * Nby) + 1); n++) {

            if (n < ((nbx * nby) + 1)) {
                newlabel[n] = label[n];
                newpreserved[n] = preserved[n];
                newcolor[n] = color[n];
            } else {

                buttonTextArrayList.add(new Integer(n + 1));
                newlabel[n] = new String("Label " + n);

                // newcolor[n] = nullColor;
                newcolor[n] = null;
                newpreserved[n] = false;

            }
        }

        nbx = Nbx;
        nby = Nby;
        label = newlabel;
        preserved = newpreserved;
        color = newcolor;


        // for resizing, we have to set the selected button color to null if there is no paint of it
        ModelImage imgB = image.getParentFrame().getImageB();
        boolean test = false;

        if (!image.isColorImage()) {

            for (int i = 0; i < imgBSize; i++) {

                if (imgB.getUByte(i) == selected) {
                    test = true;

                    break;
                }
            }
        } else {
        	boolean match = false;
            for (int i = 0; i < (imgBSize * 4); i = i + 4) {
                short r, g, b;
                int k;
                r = imgB.getUByte(i + 1);
                g = imgB.getUByte(i + 2);
                b = imgB.getUByte(i + 3);

                if ((r != 0) || (g != 0) || (b != 0)) {


                    for (k = 0; k < (lutB.getExtents()[1] * 4); k = k + 4) {

                        if ((lutB.getUByte(k + 1) == r) && (lutB.getUByte(k + 2) == g) && (lutB.getUByte(k + 3) == b)) {
                            break;
                        }
                    }
                    if(match) {
	                    int val = k / 4;
	
	                    if (val == selected) {
	                        test = true;
	
	                        break;
	                    }
                    }
                    match = false;
                }
            }
        }

        if (!test) {

            try {
                color[selected] = null;
            } catch (ArrayIndexOutOfBoundsException e) { }
        }

    }

    /**
     * Refreshes the displayed paint mask.
     *
     * @param  img  DOCUMENT ME!
     * @param  obj  DOCUMENT ME!
     */
    private void refreshImagePaint(ModelImage img, BitSet obj) {

        // replace it by previous
        img.getParentFrame().getComponentImage().setPaintMask(obj);
        img.setMask(obj);

        // show result
        img.getParentFrame().updateImages(true);

        if (img.getTriImageFrame() != null) {
            for (int i = 0; i < img.getTriImageFrame().MAX_TRI_IMAGES; i++) {
                if (img.getTriImageFrame().triImage[i] != null) {
                	img.getTriImageFrame().triImage[i].setPaintMask(obj);
                }
            }
            img.getTriImageFrame().updateImages(true);
        }
    }

    

    /**
     * Reinstantiates the labels for redisplay. Purpose: unknown
     */
    private void refreshLabelDisplay() {


        multiButton = new BorderedButton[(nbx * nby) + 1];
        listButton = new BorderedButton[(nbx * nby) + 1];
        preserveBox = new JCheckBox[(nbx * nby) + 1];
        labelField = new JTextField[(nbx * nby) + 1];
        
        //if when resizing and you resize to a number thats equal to or higher than the largets number in the buttonTextArrayList,
        //then reset everything
        int highestNum=0;
        for(int i=0;i<buttonTextArrayList.size();i++) {
        	if(Integer.parseInt(buttonTextArrayList.get(i).toString()) > highestNum) {
        		highestNum = Integer.parseInt(buttonTextArrayList.get(i).toString());
        	}
        }
        boolean flag = false;
        if((nbx * nby) <= highestNum) {
        	flag = true;
        	color[1] = lutB.getColor(1);
        	for (int k=2;k<color.length;k++) {
        		color[k] = null;
        	}
        	
        }

        
        
        for (int n = 1; n < ((nbx * nby) + 1); n++) {
            labelField[n] = new JTextField(5);
            labelField[n].setText(label[n]);
            labelField[n].setFont(serif12);
            labelField[n].addActionListener(this);
            labelField[n].setActionCommand("Label " + n);

            if (!flag) {
                multiButton[n] = new BorderedButton(((Integer) buttonTextArrayList.get(n - 1)).toString());
            } else {
                multiButton[n] = new BorderedButton(String.valueOf(n));
            }

            multiButton[n].setName("multiButton:" + n);
            multiButton[n].addActionListener(this);
            multiButton[n].addMouseListener(this);
            multiButton[n].setActionCommand("PaintMask " + n);
            multiButton[n].setFont(MipavUtil.font10);
            multiButton[n].setMaximumSize(new Dimension(48, 20));
            multiButton[n].setPreferredSize(new Dimension(48, 20));

            if (!flag) {
                listButton[n] = new BorderedButton(((Integer) buttonTextArrayList.get(n - 1)).toString());
            } else {
                listButton[n] = new BorderedButton(String.valueOf(n));
            }

            listButton[n].setName("listButton:" + n);
            listButton[n].addActionListener(this);
            listButton[n].addMouseListener(this);
            listButton[n].setActionCommand("PaintMask " + n);
            listButton[n].setFont(MipavUtil.font10);
            listButton[n].setMaximumSize(new Dimension(50, 18));
            listButton[n].setPreferredSize(new Dimension(50, 18));

            preserveBox[n] = new JCheckBox("");
            preserveBox[n].addActionListener(this);
            preserveBox[n].setActionCommand("Preserve " + n);
            preserveBox[n].setToolTipText("Lock the paint mask");

            if (n <= buttonTextArrayList.size()) {
                buttonTextArrayList.set(n - 1, new Integer(multiButton[n].getText()));
            } else {
                buttonTextArrayList.add(new Integer(multiButton[n].getText()));
            }
        }

        mainPanel.remove(multiPanel);

        multiPanel = new JPanel(new GridBagLayout());
        multiPanel.setName("multiPanel");
        multiPanel.setBorder(buildTitledBorder("Mask Palette"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0, 0, 0, 0);

        for (int i = 0; i < nbx; i++) {

            for (int j = 0; j < nby; j++) {
                gbc.gridx = i;
                gbc.gridy = j;
                multiPanel.add(multiButton[i + (nbx * j) + 1], gbc);
            }
        }

        mainPanel.remove(scrollPane);
        mainPanel.remove(lockPanel);

        listPanel = new JPanel(new GridBagLayout());
        listPanel.setName("listPanel");
        listPanel.setBorder(buildTitledBorder("Label list"));


        for (int i = 0; i < nbx; i++) {

            for (int j = 0; j < nby; j++) {
                gbc.gridy = i + (nbx * j);
                gbc.gridwidth = 1;
                gbc.gridx = 0;
                gbc.weightx = 0;
                gbc.fill = GridBagConstraints.NONE;
                listPanel.add(listButton[i + (nbx * j) + 1], gbc);
                gbc.gridx = 1;
                gbc.weightx = 1;
                gbc.fill = GridBagConstraints.HORIZONTAL;
                listPanel.add(labelField[i + (nbx * j) + 1], gbc);
                gbc.gridx = 2;
                gbc.weightx = 0;
                gbc.fill = GridBagConstraints.NONE;
                listPanel.add(preserveBox[i + (nbx * j) + 1], gbc);
            }
        }
        
        lockPanel = new JPanel();
        lockAllButton = new JButton("lock all masks");
        lockAllButton.addActionListener(this);
        lockAllButton.setActionCommand("lockAll");
        gbc.gridy = 0;
        gbc.gridx = 0;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.insets = new Insets(5, 2, 5, 2);
        lockPanel.add(lockAllButton,gbc);
        
        unlockAllButton = new JButton("unlock all masks");
        unlockAllButton.addActionListener(this);
        unlockAllButton.setActionCommand("unlockAll");
        gbc.gridy = 0;
        gbc.gridx = 1;
        lockPanel.add(unlockAllButton,gbc);
        lockPanel.setVisible(false);

        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 0, 0, 0);

        scrollPane = new JScrollPane(listPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setPreferredSize(new Dimension(400, 560));
        scrollPane.setName("scrollPane");
        scrollPane.setVisible(false);

        gbc.gridx = 0;
        gbc.gridwidth = 1;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        gbc.weighty = 1;
        mainPanel.add(multiPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(scrollPane, gbc);
        gbc.gridy = 3;
        mainPanel.add(lockPanel, gbc);

        if (displayModeButton.isSelected()) {
            multiPanel.setVisible(false);
            scrollPane.setVisible(true);
            lockPanel.setVisible(true);
        } else {
            scrollPane.setVisible(false);
            lockPanel.setVisible(false);
            multiPanel.setVisible(true);
        }

        imageBInit();

        pack();
        repaint();
    }

    /**
     * Used to reset the button labels to their default setting. Currently not used.
     *
     * @param  Nbx  number of labels in the x-direction
     * @param  Nby  number of labels in the y-direction
     */
    private void resetLabelList(int Nbx, int Nby) {
        String[] newlabel = new String[(Nbx * Nby) + 1];
        JTextField[] newlabelField = new JTextField[(Nbx * Nby) + 1];
        BorderedButton[] newmultiButton = new BorderedButton[(Nbx * Nby) + 1];
        BorderedButton[] newlistButton = new BorderedButton[(Nbx * Nby) + 1];
        boolean[] newpreserved = new boolean[(Nbx * Nby) + 1];
        Color[] newcolor = new Color[(Nbx * Nby) + 1];
        JCheckBox[] newpreserveBox = new JCheckBox[(Nbx * Nby) + 1];

        for (int n = 1; n < ((Nbx * Nby) + 1); n++) {

            if (n < ((nbx * nby) + 1)) {
                newlabel[n] = label[n];
                newlabelField[n] = labelField[n];
                newmultiButton[n] = multiButton[n];
                newlistButton[n] = listButton[n];
                newpreserved[n] = preserved[n];
                newcolor[n] = color[n];
                newpreserveBox[n] = preserveBox[n];
            } else {
                newlabel[n] = new String("Label " + n);
                newlabelField[n] = new JTextField(5);
                newlabelField[n].setText(newlabel[n]);
                newlabelField[n].setFont(serif12);
                newlabelField[n].addActionListener(this);
                newlabelField[n].setActionCommand("Label " + n);

                newmultiButton[n] = new BorderedButton(String.valueOf(n));
                newmultiButton[n].addActionListener(this);
                newmultiButton[n].setActionCommand("PaintMask " + n);
                newmultiButton[n].setFont(MipavUtil.font10);
                newmultiButton[n].setSelected(false);
                newmultiButton[n].setMaximumSize(new Dimension(48, 20));
                newmultiButton[n].setPreferredSize(new Dimension(48, 20));
                newmultiButton[n].setToolTipText(newlabel[n]);

                newlistButton[n] = new BorderedButton(String.valueOf(n));
                newlistButton[n].addActionListener(this);
                newlistButton[n].setActionCommand("PaintMask " + n);
                newlistButton[n].setFont(MipavUtil.font10);
                newlistButton[n].setSelected(false);
                newlistButton[n].setMaximumSize(new Dimension(50, 18));
                newlistButton[n].setPreferredSize(new Dimension(50, 18));
                
                newcolor[n] = null;

                newpreserved[n] = false;
                newpreserveBox[n] = new JCheckBox("");
                newpreserveBox[n].addActionListener(this);
                newpreserveBox[n].setActionCommand("Preserve " + n);
                newpreserveBox[n].setToolTipText("Lock the paint mask");
            }
        }

        nbx = Nbx;
        nby = Nby;
        label = newlabel;
        labelField = newlabelField;
        multiButton = newmultiButton;
        listButton = newlistButton;
        preserved = newpreserved;
        color = newcolor;
        preserveBox = newpreserveBox;

        mainPanel.remove(multiPanel);

        multiPanel = new JPanel(new GridBagLayout());
        multiPanel.setBorder(buildTitledBorder("Mask Palette"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0, 0, 0, 0);

        for (int i = 0; i < nbx; i++) {

            for (int j = 0; j < nby; j++) {
                gbc.gridx = i;
                gbc.gridy = j;
                multiPanel.add(multiButton[i + (nbx * j) + 1], gbc);
            }
        }

        mainPanel.remove(scrollPane);
        mainPanel.remove(lockPanel);

        listPanel = new JPanel(new GridBagLayout());
        listPanel.setBorder(buildTitledBorder("Label list"));

        for (int i = 0; i < nbx; i++) {

            for (int j = 0; j < nby; j++) {
                gbc.gridy = i + (nbx * j);
                gbc.gridwidth = 1;
                gbc.gridx = 0;
                gbc.weightx = 0;
                gbc.fill = GridBagConstraints.NONE;
                listPanel.add(listButton[i + (nbx * j) + 1], gbc);
                gbc.gridx = 1;
                gbc.weightx = 1;
                gbc.fill = GridBagConstraints.HORIZONTAL;
                listPanel.add(labelField[i + (nbx * j) + 1], gbc);
                gbc.gridx = 2;
                gbc.weightx = 0;
                gbc.fill = GridBagConstraints.NONE;
                listPanel.add(preserveBox[i + (nbx * j) + 1], gbc);
            }
        }
        
        JPanel lockPanel = new JPanel();
        lockAllButton = new JButton("lock all masks");
        lockAllButton.addActionListener(this);
        lockAllButton.setActionCommand("lockAll");
        gbc.gridy = 0;
        gbc.gridx = 0;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.insets = new Insets(5, 2, 5, 2);
        lockPanel.add(lockAllButton,gbc);
        
        unlockAllButton = new JButton("unlock all masks");
        unlockAllButton.addActionListener(this);
        unlockAllButton.setActionCommand("unlockAll");
        gbc.gridy = 0;
        gbc.gridx = 1;
        lockPanel.add(unlockAllButton,gbc);
        lockPanel.setVisible(false);

        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 0, 0, 0);

        scrollPane = new JScrollPane(listPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        scrollPane.setPreferredSize(new Dimension(400, 560));
        scrollPane.setName("scrollPane");
        scrollPane.setVisible(false);

        gbc.gridx = 0;
        gbc.gridwidth = 1;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        gbc.weighty = 1;
        mainPanel.add(multiPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(scrollPane, gbc);
        gbc.gridy = 3;
        mainPanel.add(lockPanel, gbc);

        if (displayModeButton.isSelected()) {
            multiPanel.setVisible(false);
            scrollPane.setVisible(true);
            lockPanel.setVisible(true);
        } else {
            scrollPane.setVisible(false);
            lockPanel.setVisible(false);
            multiPanel.setVisible(true);
        }

        pack();
        repaint();
    }

    /**
     * Handles the action event generated by the "Save label file" dialog. Calls readLabelsFromFile(String) to save the
     * labels to the file selected by the user.
     *
     * @param  evt  the ActionEvent generated by this dialog
     */
    private void saveFileActionPerformed(ActionEvent evt) {

        if (JFileChooser.APPROVE_SELECTION.equals(evt.getActionCommand())) {
            String filename = saveDialog.getSelectedFile().getAbsolutePath();
            writeLabelsToFile(filename);
            userInterface.setGlobalDataText("label data saved to " + filename + "\n");
        }
        // saveDialog.setVisible(false);
    }

    /**
     * Converts the selected mask to paint.
     *
     * @param  num  the index into the color array, which indicates the color of the paint
     */
    private void selectedMaskToPaint(int num) {

        if (image == null) {
            System.gc();
            MipavUtil.displayError("MultiPaint error: image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("MultiPaint error: paint mask not found");

            return;
        }

        // select the ID: if not new, update the mask
        if (image.getParentFrame().getImageB() == null) {

            // create the mask image
            image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "NewMask"));
        }

        // record selected image; set to image B
        ModelImage active = image.getParentFrame().getActiveImage();

        image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_B);

        // create new color
        color[num] = lutB.getColor(num);
        multiButton[num].setBackground(color[num]);
        listButton[num].setBackground(color[num]);

        // call the paint to mask program for exiting mask
        image.getParentFrame().getComponentImage().setIntensityDropper((float) num);
        image.getParentFrame().getControls().getTools().setPaintColor(color[num]);

        image.getParentFrame().getComponentImage().updatePaintBrushCursor();
        
        if (image.getTriImageFrame() != null) {
            image.getTriImageFrame().setIntensityDropper((float) num);
            image.getTriImageFrame().setPaintColor(color[num]);

        }

        image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "MaskToPaint"));

        // reset the active image and intensity label
        if (!active.equals(image.getParentFrame().getActiveImage())) {
            image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_A);
        }

        selected = num;
        multiButton[selected].setSelected(true);
        listButton[selected].setSelected(true);

        refreshImagePaint(image, obj);
    }


    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * Converts paint to a mask, then mask to paint. Reason: unknown.
     *
     * @param  from  DOCUMENT ME!
     * @param  to    DOCUMENT ME!
     */
    private void switchPaintAndMask(int from, int to, int colorNum) {
        multiButton[from].setSelected(false);
        listButton[from].setSelected(false);

        if (image == null) {
            System.gc();
            MipavUtil.displayError("Error: image not found");

            return;
        }

        // retrieve the mask
        BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();

        if (obj == null) {
            MipavUtil.displayError("Error: paint mask not found");

            return;
        }

        // select the ID: if not new, update the mask
        if (image.getParentFrame().getImageB() == null) {

            // create the mask image
            image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "NewMask"));
        }

        // record selected image; set to image B
        ModelImage active = image.getParentFrame().getActiveImage();
        image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_B);

        // set the color of the button being switched to

        //if (color[to] == null) {
            color[to] = lutB.getColor(colorNum);
        //}

        multiButton[to].setBackground(color[to]);
        listButton[to].setBackground(color[to]);

        if (indeterminateProgressBar != null) {
            indeterminateProgressBar.setIndeterminate(true);
        }

        // must convert variables to final for use in inner class
        final int _from = from;
        final int _to = to;
        final ModelImage imageB = image.getParentFrame().getImageB();

        image.getParentFrame().getComponentImage().setModifyFlag(false);
        
        
        Thread thread = new Thread() {
            public void run() {

                // call the paint to mask program for existing mask
                image.getParentFrame().getComponentImage().setIntensityDropper((float) (new Integer(multiButton[_from].getText()).intValue()));
                image.getParentFrame().getComponentImage().commitMask(imageB, true, true, intensityLockVector, false);

                // call the mask to paint program for starting mask
                if (color[_to] == null) {
                    color[_to] = lutB.getColor(_to);
                }

                image.getParentFrame().getComponentImage().setIntensityDropper((float) (new Integer(multiButton[_to].getText()).intValue()));
                image.getParentFrame().getControls().getTools().setPaintColor(color[_to]);

                image.getParentFrame().getComponentImage().updatePaintBrushCursor();
                
                if (image.getTriImageFrame() != null) {
                    image.getTriImageFrame().setIntensityDropper((float) (new Integer(multiButton[_to].getText()).intValue()));
                    image.getTriImageFrame().setPaintColor(color[_to]);

                }

                ((ViewJFrameImage) image.getParentFrame()).handleMaskToPaint(false);

                if (indeterminateProgressBar != null) {
                    indeterminateProgressBar.setIndeterminate(false);
                }

                image.getParentFrame().getComponentImage().setModifyFlag(true);

                image.notifyImageDisplayListeners();
            }
        };

        thread.start();

        // reset the active image and intensity label
        if (!active.equals(image.getParentFrame().getActiveImage())) {
            image.getParentFrame().setActiveImage(ViewJFrameBase.IMAGE_A);
        }

        selected = to;
        multiButton[selected].setSelected(true);
        listButton[selected].setSelected(true);

        refreshImagePaint(image, obj);
    }

    /**
     * Writes the 'labels' file to disk.
     *
     * @param  filename  DOCUMENT ME!
     */
    private void writeLabelsToFile(String filename) {

        try {

            // open the file for writing
            File f = new File(filename);
            FileWriter fw = new FileWriter(f);
            PrintWriter pw = new PrintWriter(fw);

            // write the parameters
            pw.println("Labels for MultiPaint (edit with care)");
            pw.println("Number of labels: " + nbx + " x " + nby);
            pw.println("Id:buttonText:name:color");

            for (int n = 1; n < ((nbx * nby) + 1); n++) {

                // make sure the labels are consistent with the display
                label[n] = labelField[n].getText();

                int maskNum = new Integer(listButton[n].getText()).intValue();

                // I wanted to use  "|" as a separator but the split() did not like to split it using that so i used ":"
                if (color[n] == null) {
                    pw.println("" + n + ":" + maskNum + ":" + label[n] + ":null");
                } else {
                    pw.println("" + n + ":" + maskNum + ":" + label[n] + ":" + color[n].getRGB());
                }
            }

            // close the file
            fw.close();
        } catch (IOException ioe) {
            Preferences.debug(ioe.getMessage());
        }

    }


}
