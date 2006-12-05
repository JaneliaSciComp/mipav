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
public class JDialogMultiPaint extends JDialogBase implements MouseListener{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7497259210728078264L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int alphaBlend = 50;

    /** DOCUMENT ME! */
    private JPanel bottomPanel;

    /** array of colors to use for the labels */
    private Color[] color;

    /** private String title; private boolean useTriplanar=false;. */
    private BitSet currentMask;

    /** display masks toggle button */
    private JToggleButton displayMasksButton;

    /** DOCUMENT ME! */
    private JToggleButton displayModeButton;

    /** DOCUMENT ME! */
    private JToggleButton displayPaintButton;

    /** DOCUMENT ME! */
    private JPanel displayPanel;

    /** DOCUMENT ME! */
    private JCheckBox editBox;

    /** DOCUMENT ME! */
    private JPanel filePanel;

    /** DOCUMENT ME! */
    private JPanel voiPanel;

    /** source image */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JProgressBar indeterminateProgressBar = new JProgressBar();

    /** DOCUMENT ME! */
    private Vector intensityLockVector = new Vector();

    /** labels for the painted objects */
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
    private BorderedButton[] multiButton;

    /** DOCUMENT ME! */
    private JPanel multiPanel;

    /** number of paint masks initially */
    private int nbx = 4, nby = 6;

    /**
     * private boolean displayMask,displayPaint; // check whether the mask and paint are displayed private boolean
     * editable; // wether or not you can edit the object names.
     */
    //private Color nullColor = Color.gray;

    /** DOCUMENT ME! */
    private JLabel numberLabel;

    /** DOCUMENT ME! */
    private JPanel numberPanel;

    /** resize x value */
    private JTextField numberXField;

    /** resize y value */
    private JTextField numberYField;

    /** DOCUMENT ME! */
    private JPanel optionPanel;

    /** DOCUMENT ME! */
    private JCheckBox[] preserveBox;

    /** check whether the mask can be affected by new paint or not */
    private boolean[] preserved; 

    /** resize button */
    private JButton resizeButton;

    /** DOCUMENT ME! */
    private JFileChooser saveDialog;

    /** save labels button */
    private JButton saveLabelsButton;

    /** save mask button */
    private JButton saveMaskButton;

    /** id for the selected paint mask */
    private int selected = 1;

    /** private int destExtents[];. */
    private ViewUserInterface userInterface;
    
    /** this is the array list of texts for the mask number buttons */
    private ArrayList buttonTextArrayList;
    
    
    /** The mask size in the x dimension. */
    private int xDim;
    
    /** The mask size in the y dimension. */
    private int yDim;
    
    /** The mask size in the z dimension. */
    private int zDim;
    
    /** The size, in voxels, of the mask. */
    private int imgBSize;
    
    /** */
    private static ModelLUT lutB;
    
	/** button VOI import */
    private JButton importVoiButton;

    /** button VOI export */
    private JButton exportVoiButton;

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

    /**
     * Creates a new JDialogMultiPaint object.
     *
     * @deprecated  Used primarily for the script to store variables and run the algorithm. No actual dialog will appear
     *              but the set up info and result image will be stored here.
     *
     * @param       UI  The user interface, needed to create the image frame.
     * @param       im  Source image.
     */
    public JDialogMultiPaint(ViewUserInterface UI, ModelImage im) {
        this(im);
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
        	//we need to commit the paint to mask
        	BitSet obj = image.getParentFrame().getComponentImage().getPaintMask();
        	ModelImage imgB = image.getParentFrame().getImageB();
        	image.getParentFrame().getComponentImage().setIntensityDropper((float) selected);
            image.getParentFrame().getComponentImage().commitMask(imgB, true, true, intensityLockVector, false);
            image.getParentFrame().getComponentImage().setModifyFlag(true);
            image.notifyImageDisplayListeners();
            refreshImagePaint(image, obj);
            dispose();
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("10027");
        } else if (command.startsWith("PaintMask")) {

            // System.out.println("(selected: "+selected);
            int num = Integer.valueOf(command.substring(command.indexOf(" ") + 1)).intValue();

            // convert the paint to previous selection
            // and the newly selected mask to paint
            switchPaintAndMask(selected, num);
        } else if (command.startsWith("Preserve")) {
            int num = Integer.valueOf(command.substring(command.indexOf(" ") + 1)).intValue();

            // System.out.println("preserve "+num);
            if (preserveBox[num].isSelected()) {
                preserved[num] = true;

                if ((parentFrame != null) && (parentFrame instanceof ViewJFrameImage)) {
                    addIntensityLock(num);
                }
            } else {
                preserved[num] = false;

                if ((parentFrame != null) && (parentFrame instanceof ViewJFrameImage)) {
                    removeIntensityLock(num);
                }
            }
        } else if (command.startsWith("Label")) {
            int num = Integer.valueOf(command.substring(command.indexOf(" ") + 1)).intValue();

            // System.out.println("label "+num);
            label[num] = labelField[num].getText();
            multiButton[num].setToolTipText(label[num]);
        } else if (command.equals("SwitchMode")) {

            if (displayModeButton.isSelected()) {
                multiPanel.setVisible(false);
                listPanel.setVisible(true);
            } else {

                // refresh the text tooltips
                for (int n = 1; n < ((nbx * nby) + 1); n++) {
                    label[n] = labelField[n].getText();
                    multiButton[n].setToolTipText(label[n]);
                }

                listPanel.setVisible(false);
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

            // resetLabelList(Nbx,Nby);
            newLabelList(Nbx, Nby);
            refreshLabelDisplay();

        } else if (command.equals("HidePaint")) {

            if (displayPaintButton.isSelected()) {
                currentMask = (BitSet) image.getParentFrame().getComponentImage().getPaintMask().clone();
                refreshImagePaint(image, new BitSet());
            } else {
                refreshImagePaint(image, currentMask);
            }
        } else if (command.equals("HideMasks")) {

            if (displayMasksButton.isSelected()) {
                alphaBlend = (int) (image.getParentFrame().getComponentImage().getAlphaBlend() * 100.0f);

                // System.out.print(" alpha: "+alphaBlend);
                image.getParentFrame().getComponentImage().setAlphaBlend(100);
            } else {
                image.getParentFrame().getComponentImage().setAlphaBlend(alphaBlend);
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
            selectedMaskToPaint(1);
        } else if (command.equals("ImportVOI")) {
            deselectMask();
            /** I changed this to onlyActive is false... you can switch*/
            image.getParentFrame().setImageB(image.generateUnsignedByteImage(1, false, false));
			image.getParentFrame().getLUTb().makeStripedLUT();
			// import the VOI labels as well
			VOIVector voi = image.getVOIs();
			int Nlabel = voi.size();
			for (int n=0;n<Nlabel ;n++) {
				label[n+1] = voi.VOIAt(n).getName();
			}
			selectedMaskToPaint(1);
        } else if (command.equals("ExportVOI")) {
            int num = selected;
            commitPaintToMask(num);
            // empty VOI set to start
			image.getParentFrame().getImageB().resetVOIs();
			
            AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(image.getParentFrame().getImageB());

            String [] correctedLabels = new String[label.length - 1];
            for (int i = 0; i < correctedLabels.length; i++) {
            	correctedLabels[i] = new String(label[i+1]);
            }
            
            VOIExtractionAlgo.setNameTable(correctedLabels);
            VOIExtractionAlgo.run();
			// transfer the VOIs to the original image (and keep in the mask)
			VOIVector voi = image.getParentFrame().getImageB().getVOIs();
			image.resetVOIs();
			image.setVOIs(voi);
			deselectMask();
            selectedMaskToPaint(num);
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
                    num = Integer.valueOf(line.substring(0, line.indexOf("|") - 1)).intValue();

                    if (num == n) {
                        label[n] = line.substring(line.indexOf("|") + 2, line.lastIndexOf("|") - 1);
                        color[n] = new Color(Integer.valueOf(line.substring(line.lastIndexOf("|") + 2)).intValue());
                        
                    }
                    */
                    String[] maskLabelInfo = line.split(":");
                    if(maskLabelInfo.length == 4) {
                    	num = new Integer(maskLabelInfo[0].trim()).intValue();
                    }
                    else {
                        num = Integer.valueOf(line.substring(0, line.indexOf("|") - 1)).intValue();
                    }
                    if (num == n) {
                    	if (maskLabelInfo.length == 4) {
                    		buttonTextArrayList.set(n-1, new Integer(maskLabelInfo[1].trim()));
                    		label[n] =  maskLabelInfo[2].trim();
                    		if (maskLabelInfo[3].trim().equals("null")) {
                    			color[n] = null;
                    		}
                    		else {
                    			color[n] = new Color(new Integer(maskLabelInfo[3].trim()).intValue());
                    		}
                    	}
                    	//need to handle the old labels files also since i added another column for button text
                    	else {
                    		buttonTextArrayList.set(n-1, new Integer(n));
                            label[n] = line.substring(line.indexOf("|") + 2, line.lastIndexOf("|") - 1);
                            //since the old files were saving the grey value of the button, we need to check b/c we dont
                            //want the buttons grey
                            if (Integer.valueOf(line.substring(line.lastIndexOf("|") + 2)).intValue() == -8355712) {
                            	color[n] = null;
                            }
                            else {
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
        color[num] = lutB.getColor(num);
        multiButton[num].setBackground(color[num]);
        listButton[num].setBackground(color[num]);

        // call the paint to mask program for exiting mask
        image.getParentFrame().getComponentImage().setIntensityDropper((float) num);
        image.getParentFrame().actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "CommitPaint"));

        selected = 0;

        refreshImagePaint(image, obj);
    }

    /**
     * Sets buttons to deselected, then calls refreshImagePaint to reset paint as mask.
     */
    private void deselectMask() {
        if (selected>0) {
			multiButton[selected].setSelected(false);
			listButton[selected].setSelected(false);
		}
        selected = 0;
        refreshImagePaint(image, new BitSet());
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
            //multiButton[n].setToolTipText(label[n]);

            listButton[n] = new BorderedButton(String.valueOf(n));
            listButton[n].setName("listButton:" + n);
            listButton[n].addActionListener(this);
            listButton[n].addMouseListener(this);
            listButton[n].setActionCommand("PaintMask " + n);
            listButton[n].setFont(MipavUtil.font10);
            listButton[n].setSelected(false);
            listButton[n].setMaximumSize(new Dimension(50, 18));
            listButton[n].setPreferredSize(new Dimension(50, 18));
            //listButton[n].setBackground(nullColor);

            //color[n] = nullColor;
            color[n] = null;

            preserved[n] = false;
            preserveBox[n] = new JCheckBox("");
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

        listPanel.setVisible(false);

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

        filePanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        filePanel.add(loadLabelsButton, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        filePanel.add(saveLabelsButton, gbc);
        gbc.gridx = 2;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        // filePanel.add(editBox, gbc);

        maskPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanel.add(loadMaskButton, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanel.add(saveMaskButton, gbc);

        voiPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        voiPanel.add(importVoiButton, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        voiPanel.add(exportVoiButton, gbc);

        displayPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        displayPanel.add(displayModeButton, gbc);
        gbc.gridx = 1;
        displayPanel.add(displayPaintButton, gbc);
        gbc.gridx = 2;
        displayPanel.add(displayMasksButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        optionPanel.add(numberPanel, gbc);
        gbc.gridy = 1;
        optionPanel.add(filePanel, gbc);
        gbc.gridy = 2;
        optionPanel.add(maskPanel, gbc);
        gbc.gridy = 3;
        optionPanel.add(voiPanel, gbc);
        gbc.gridy = 4;
        optionPanel.add(displayPanel, gbc);

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
        mainPanel.add(listPanel, gbc);

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
        
        //getting image b size
        ModelImage imgB = image.getParentFrame().getImageB();
        int numDims = imgB.getNDims();
        
        xDim = imgB.getExtents()[0];
        yDim = imgB.getExtents()[1];
        if(numDims == 2) {
        	imgBSize = xDim * yDim;
        }
        else {
        	zDim = imgB.getExtents()[2];
        	imgBSize = xDim * yDim * zDim;
        }
        
        //this boolean is to test whether image b is blank or not...initally set to true
        boolean blankImage = true;
        //loops through the image b and gets ubytes
        if(!image.isColorImage()) {
	        for (int i = 0;i < imgBSize; i++) {
	        	if(imgB.getUByte(i) != 0) {
	        		//there is at least some mask...so set blank image to flase
	        		blankImage = false;
	        		int val = imgB.getUByte(i);
	        		color[val] = lutB.getColor(val);
	        		multiButton[val].setBackground(color[val]);
	        		listButton[val].setBackground(color[val]);
	        		
	        	}
	        }
        }
        else {
        	for (int i = 1;i < imgBSize*4; i= i+4) {
        		short r,g,b;
        		int k;
        		r = imgB.getUByte(i);
        		g = imgB.getUByte(i+1);
        		b = imgB.getUByte(i+2) ;
      
	        	if(r != 0 || g != 0 || b != 0) {
	        		//there is at least some mask...so set blank image to flase
	        		blankImage = false;
	        		
	        		for(k=1;k<lutB.getExtents()[1]*4;k=k+4) {
	        			if(lutB.getUByte(k) == r && lutB.getUByte(k+1) == g && lutB.getUByte(k+2) == b) {
	        				break;
	        			}
	        		}
	        		int val = k/4;
	        		color[val] = lutB.getColor(val);
	        		multiButton[val].setBackground(color[val]);
	        		listButton[val].setBackground(color[val]);
	        		
	        	}
	        }
        }
        //since the image b is not blank, we need to figure out which button should be selected
        if(!blankImage) {
	        for (int n = 1; n <= (nbx * nby); n++) {
	        	if(color[n] == null) {
	        		selected = n;
	        		color[n] = lutB.getColor(n);
	        		multiButton[n].setBackground(color[n]);
	        		multiButton[n].setSelected(true);
	        		multiButton[1].setSelected(false);
	        		listButton[n].setBackground(color[n]);
	        		listButton[n].setSelected(true);
	        		listButton[1].setSelected(false);
	        		image.getParentFrame().getComponentImage().setIntensityDropper((float) n);
	                image.getParentFrame().getControls().getTools().setPaintColor(color[n]);
	        		break;
	        	}
	        	
	        }
        }
        
        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
       
    } // end init()

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
        
        //set up LUTb
        if(image.isColorImage()) {
        	//need to instantiate a LUTb
        	lutB = new ModelLUT(ModelLUT.STRIPED, 256, new int[] { 4, 256 });
        }
        else {
        	lutB = image.getParentFrame().getLUTb();
        }

        // create new color
        color[num] = lutB.getColor(num);
        multiButton[num].setBackground(color[num]);
        listButton[num].setBackground(color[num]);
        	

        // call the paint to mask program for exiting mask
        image.getParentFrame().getComponentImage().setIntensityDropper((float) num);
        image.getParentFrame().getControls().getTools().setPaintColor(color[num]);

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

        for (int n = 1; n < ((Nbx * Nby) + 1); n++) {

            if (n < ((nbx * nby) + 1)) {
                newlabel[n] = label[n];
                newpreserved[n] = preserved[n];
                newcolor[n] = color[n];
            } else {
                newlabel[n] = new String("Label " + n);
                //newcolor[n] = nullColor;
                newcolor[n] = null;
                newpreserved[n] = false;
            }
        }

        nbx = Nbx;
        nby = Nby;
        label = newlabel;
        preserved = newpreserved;
        color = newcolor;
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
            img.getTriImageFrame().getTriImage(0).setPaintMask(obj);
            img.getTriImageFrame().getTriImage(1).setPaintMask(obj);
            img.getTriImageFrame().getTriImage(2).setPaintMask(obj);
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

        for (int n = 1; n < ((nbx * nby) + 1); n++) {
            labelField[n] = new JTextField(5);
            labelField[n].setText(label[n]);
            labelField[n].setFont(serif12);
            labelField[n].addActionListener(this);
            labelField[n].setActionCommand("Label " + n);
            if(n < buttonTextArrayList.size()) {
            	multiButton[n] = new BorderedButton(((Integer)buttonTextArrayList.get(n-1)).toString());
            }
            else {
            	multiButton[n] = new BorderedButton(String.valueOf(n));
            }
            multiButton[n].setName("multiButton:" + n);
            multiButton[n].addActionListener(this);
            multiButton[n].addMouseListener(this);
            multiButton[n].setActionCommand("PaintMask " + n);
            multiButton[n].setFont(MipavUtil.font10);
            multiButton[n].setSelected(false);
            multiButton[n].setMaximumSize(new Dimension(48, 20));
            multiButton[n].setPreferredSize(new Dimension(48, 20));
            //multiButton[n].setToolTipText(label[n]);
            multiButton[n].setBackground(color[n]);


            if(n < buttonTextArrayList.size()) {
            	listButton[n] = new BorderedButton(((Integer)buttonTextArrayList.get(n-1)).toString());
            }
            else {
            	listButton[n] = new BorderedButton(String.valueOf(n));
            }
            listButton[n].setName("listButton:" + n);
            listButton[n].addActionListener(this);
            listButton[n].addMouseListener(this);
            listButton[n].setActionCommand("PaintMask " + n);
            listButton[n].setFont(MipavUtil.font10);
            listButton[n].setSelected(false);
            listButton[n].setMaximumSize(new Dimension(50, 18));
            listButton[n].setPreferredSize(new Dimension(50, 18));
            listButton[n].setBackground(color[n]);


            preserveBox[n] = new JCheckBox("");
            preserveBox[n].addActionListener(this);
            preserveBox[n].setActionCommand("Preserve " + n);
            preserveBox[n].setToolTipText("Lock the paint mask");
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

        mainPanel.remove(listPanel);

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

        gbc.gridx = 0;
        gbc.gridwidth = 1;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        gbc.weighty = 1;
        mainPanel.add(multiPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(listPanel, gbc);

        if (displayModeButton.isSelected()) {
            multiPanel.setVisible(false);
            listPanel.setVisible(true);
        } else {
            listPanel.setVisible(false);
            multiPanel.setVisible(true);
        }

        // reselect the same object
        if (selected < ((nbx * nby) + 1)) {
            multiButton[selected].setSelected(true);
            listButton[selected].setSelected(true);
        } else {
            selectedMaskToPaint(1);
        }

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

                //newcolor[n] = nullColor;
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

        mainPanel.remove(listPanel);

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

        gbc.gridx = 0;
        gbc.gridwidth = 1;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        gbc.weighty = 1;
        mainPanel.add(multiPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(listPanel, gbc);

        if (displayModeButton.isSelected()) {
            multiPanel.setVisible(false);
            listPanel.setVisible(true);
        } else {
            listPanel.setVisible(false);
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
    private void switchPaintAndMask(int from, int to) {
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

        //set the color of the button being switched to
        
        color[to] = lutB.getColor(to);

        
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
                image.getParentFrame().getComponentImage().setIntensityDropper((float) _from);
                image.getParentFrame().getComponentImage().commitMask(imageB, true, true, intensityLockVector, false);

                // call the mask to paint program for starting mask
                color[_to] = lutB.getColor(_to);
                image.getParentFrame().getComponentImage().setIntensityDropper((float) _to);
                image.getParentFrame().getControls().getTools().setPaintColor(color[_to]);
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
                //I wanted to use  "|" as a separator but the split() did not like to split it using that so i used ":"
                if (color[n] == null) {
                	pw.println("" + n + ":" + maskNum + ":" + label[n] + ":null");
                }else {
                	pw.println("" + n + ":" + maskNum + ":" + label[n] + ":" + color[n].getRGB());
                }
            }

            // close the file
            fw.close();
        } catch (IOException ioe) {
            Preferences.debug(ioe.getMessage());
        }

    }

	public void mouseClicked(MouseEvent e) {
		if (e.getButton() == MouseEvent.BUTTON3) {
			if (e.getSource() instanceof JButton) {
				JPanel correspondingPanel = null;
				String correspondingButtonName = "";
				JButton correspondingButton = null;
				JPanel muPanel = null;
				JPanel liPanel = null;
				
				//get Source Button and Source Panel
				JButton sourceButton = (JButton) e.getSource();
				JPanel srcPanel = (JPanel)sourceButton.getParent();
				String srcPanelName = srcPanel.getName();

				//get ContentPane
				Container contentPane = this.getContentPane();
				
				//get Main Panel
				JPanel mPanel = (JPanel)contentPane.getComponents()[0];

				//get Correponding Panel 
				for(int k=0;k<mPanel.getComponentCount();k++) {
					if(mPanel.getComponents()[k].getName() != null) {
						if(mPanel.getComponents()[k].getName().equals("multiPanel")) {
							muPanel = (JPanel)mPanel.getComponents()[k];
						}
						else if(mPanel.getComponents()[k].getName().equals("listPanel")) {
							liPanel = (JPanel)mPanel.getComponents()[k];
						}
					}
				}
				if (srcPanelName.equals("multiPanel")) {
					correspondingPanel = liPanel;
				}
				else if(srcPanelName.equals("listPanel")) {
					correspondingPanel = muPanel;
				}
				
				//get Corresponding Button Name
				String sourceButtonName = sourceButton.getName();
				if (sourceButtonName.split(":")[0].equals("multiButton")) {
					correspondingButtonName = "listButton:" + sourceButtonName.split(":")[1];
				}
				else {
					correspondingButtonName = "multiButton:" + sourceButtonName.split(":")[1];
				}
				
				//get Corresponding Button
				for(int i=0;i<correspondingPanel.getComponentCount();i++) {
					if (correspondingPanel.getComponents()[i].getName() != null) {
						if (correspondingPanel.getComponents()[i].getName().equals(correspondingButtonName)) {
							correspondingButton = (JButton)correspondingPanel.getComponents()[i];
						}
					}
				}
				
				JDialogChangeMaskNumber changeMaskNumberDialog = new JDialogChangeMaskNumber(sourceButton, correspondingButton, buttonTextArrayList);
				changeMaskNumberDialog.pack();
				//MipavUtil.centerOnScreen(changeMaskNumberDialog);
				MipavUtil.centerInComponent(this,changeMaskNumberDialog);
				//changeMaskNumberDialog.setAlwaysOnTop(true);
				changeMaskNumberDialog.setVisible();
			}
		}
		
	}

	public void mouseEntered(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseExited(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseReleased(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}
    
    
    
    
}
