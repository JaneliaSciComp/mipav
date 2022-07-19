package gov.nih.mipav.view.renderer.WildMagic.flythroughview;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.JPanelRendererBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.text.DecimalFormat;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;

import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;



/**
 * Control panel to set up the virtual endoscopy volume view. This panel loads the mask image and surface file. It also
 * adjusts the endoscopy view parameters.
 */
public class JPanelVirtualEndoscopySetup_WM extends JPanelRendererBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * Wrapper in order to hold the control panel layout in the JScrollPane.
     */
    class DrawingPanel extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -7360089445417194229L;

        /* (non-Javadoc)
         * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
         */
        @Override
		protected void paintComponent(Graphics g) {
            super.paintComponent(g);

        }
    }

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5676430934584329807L;

    /** Box layout for control panel. */
    private Box contentBox;

    /** Flag indicates to continue update the surface render and plane render or not. */
    private boolean continueUpdate = true;

    /** Loaded endoscopy image file. */
    private ModelImage endoscopyImage;

    /** File name and directory. */
    private String fileName, directory;

    /** Load image button. */
    private JButton flythruButtonLoadImage = new JButton();

    /** Image file name label. */
    private JLabel flythruLabelFileName = new JLabel();

    /** Button panel. */
    private JPanel kPanelButton;

    /** Flag indicates to show pseudo color or not. */
    private boolean m_bShowMeanCurvatures = false;

    /** Show curvature colors check box. */
    private JCheckBox m_kCheckBoxShowCurvatures = new JCheckBox();

    /** Surface sample reduction factor. */
    private JComboBox m_kComboSegmentSurfaceBranchSamplesReductionFactor;

    
    /** Continuous update check box. */
    private JCheckBox m_kContinueUpdate = new JCheckBox();

    /** Used to create text representations of numbers. */
    private final DecimalFormat m_kDecimalFormat;

    /** Branch label. */
    private JLabel m_kLabelBranch = new JLabel();

    /** View direction label. */
    private JLabel m_kLabelDirection = new JLabel();

    /** Path distance label. */
    private JLabel m_kLabelDistance = new JLabel();

    /** Automatically inserted by JBuilder Designer. */
    private JLabel m_kLabelFileName = new JLabel();

    /** Orientation label. */
    private JLabel m_kLabelOrientation = new JLabel();

    /** Current path position label. */
    private JLabel m_kLabelPosition = new JLabel();

    /** Step gaze label. */
    private JLabel m_kLabelStepGaze = new JLabel();

    /** Image option panel. */
    private FlyThroughRender.SetupOptions m_kOptions = new FlyThroughRender.SetupOptions();

    /** Current branch text box. */
    private JTextField m_kTextBranch = new JTextField();

    /** View direction text box. */
    private JTextField m_kTextDirection = new JTextField();

    /** Distance along path text box. */
    private JTextField m_kTextDistance = new JTextField();

    /** maximum number of branches created for the endoscopy view. */
    private JTextField m_kTextMaxNumBranches;

    /** minimum branch length. */
    private JTextField m_kTextMinBranchLength;

    /** Orientation text box. */
    private JTextField m_kTextOrientation = new JTextField();

    /** BSpline control points fraction of branch samples. */
    private JTextField m_kTextPercentBSplineNumControlPoints;

    /** Position text box. */
    private JTextField m_kTextPosition = new JTextField();

    /** Step gaze text box.*/
    private JTextField m_kTextStepGaze = new JTextField();

    /** Renderer for displaying the scene-graph. */
    private final FlyThroughRender m_kView;

    /** Scroll pane. */
    private JScrollPane scroller;

    /** Scroll panel that holding all the control components. */
    private DrawingPanel scrollPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /** continue update button. */
    private JButton updateButton = new JButton();

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates endoscopy registration control panel.
     * @param  _kView        FlythruRender refrence.
     */
    public JPanelVirtualEndoscopySetup_WM(FlyThroughRender _kView) {
        super();
        m_kView = _kView;

        init();

        // Setup to format numbers into text strings having a limited number
        // of decimal digits of precision.
        m_kDecimalFormat = new DecimalFormat();
        m_kDecimalFormat.setMinimumFractionDigits(1);
        m_kDecimalFormat.setMaximumFractionDigits(1);

    }

    /**
     * Closes dialog box when the OK button is pressed, sets up the variables needed for running the algorithm, and
     * calls the algorithm.
     * @param  event  Event that triggers function
     */
    @Override
	public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        if (source == OKButton) {

            if (setVariables()) {
                loadingImage();
                m_kCheckBoxShowCurvatures.setEnabled(true);
                OKButton.setEnabled(false);
            }
        } else if (source == cancelButton) {
            setVisible(false);
        }

        if (command.equals("Select Image ...")) {
            JFileChooser chooser = new JFileChooser();

            chooser.setMultiSelectionEnabled(true);
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            if (JFileChooser.APPROVE_OPTION != chooser.showOpenDialog(null)) {
                return;
            }

            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            flythruLabelFileName.setText(fileName);
            chooser.setVisible(false);

            if (fileName != null) {
                OKButton.setEnabled(true);
                flythruButtonLoadImage.setEnabled(false);
            }
        } else if (command.equals("Update")) {
            m_kView.update();
        }
    }

    /**
     * Dispose memory.
     * @param  flag  dispose super or not.
     */
    public void dispose(boolean flag) {
        m_kOptions = null;
        m_kTextMaxNumBranches = null;
        m_kTextMinBranchLength = null;
        m_kTextPercentBSplineNumControlPoints = null;
        m_kComboSegmentSurfaceBranchSamplesReductionFactor = null;
        m_kLabelFileName = null;
        m_kLabelDistance = null;
        m_kTextDistance = null;
        m_kLabelPosition = null;
        m_kTextPosition = null;
        m_kLabelDirection = null;
        m_kTextDirection = null;
        m_kLabelOrientation = null;
        m_kTextOrientation = null;
        m_kLabelBranch = null;
        m_kTextBranch = null;
        m_kCheckBoxShowCurvatures = null;
        m_kContinueUpdate = null;
        m_kLabelStepGaze = null;
        m_kTextStepGaze = null;
        flythruLabelFileName = null;
        flythruButtonLoadImage = null;
        endoscopyImage = null;
        scrollPanel = null;
        scroller = null;
        kPanelButton = null;
        contentBox = null;
        updateButton = null;

        if (flag == true) {
            super.disposeLocal();
        }
    }

    /**
     * Return whether to update the Volume Tri-Planar view continuously as the fly path changes.
     * @return true when updating the Volume Tri-Planar view continuously as the fly path changes.
     */
    public boolean getContinuousUpdate()
    {
        return continueUpdate;
    }

    /**
     * Return the main control panel.
     * @return  JPanel the main control panel
     */
    @Override
	public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Resize the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   control panel width
     * @param  frameHeight  control panel height
     */
    @Override
	public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - kPanelButton.getHeight()));
        scroller.setSize(new Dimension(panelWidth, frameHeight - kPanelButton.getHeight()));
        scroller.revalidate();
    }

    /**
     * Update the appropriate controls based on the current settings of the orientation-based information in the
     * FlyPathBehavior instance.
     *
     * @param  kFlyPathBehavior  FlyPathBehavior Instance which contains the current state of orientation along the
     *                           branches.
     */
    public void updateOrientation(Matrix3f kOrientation) {
        setViewOrientation(kOrientation);
    }

    /**
     * Update the appropriate controls based on the current settings of the position-based information in the
     * FlyPathBehavior instance.
     *
     * @param  kFlyPathBehavior  FlyPathBehavior Instance which contains the current state of position along the
     *                           branches.
     */
    public void updatePosition(FlyPathBehavior_WM kFlyPathBehavior) {
        setBranchInfo(kFlyPathBehavior.getBranchIndex(), kFlyPathBehavior.isPathMoveForward());
        setStepGazeDist(kFlyPathBehavior.getPathStep(), kFlyPathBehavior.getGazeDistance());
        setPathDistance(kFlyPathBehavior.getPathDistance(), kFlyPathBehavior.getPathLength());
        setPathPosition(kFlyPathBehavior.getViewPoint());
        setViewDirection(kFlyPathBehavior.getViewDirection());
    }

    /**
     * Initializes the GUI components and displays the control panel.
     */
    private void init() {

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());

        // Put the drawing area in a scroll pane.
        scroller = new JScrollPane(scrollPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        contentBox = new Box(BoxLayout.Y_AXIS);
        scrollPanel.add(contentBox, BorderLayout.NORTH);
        mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.add(scroller, BorderLayout.NORTH);

        // text box to enter max number of branches
        JLabel kLabelMaxNumBranches = new JLabel("Maximum number of branches");

        kLabelMaxNumBranches.setForeground(Color.black);
        kLabelMaxNumBranches.setFont(serif12);
        m_kTextMaxNumBranches = new JTextField(10);
        m_kTextMaxNumBranches.setText(Integer.toString(m_kOptions.m_iMaxBranches));
        m_kTextMaxNumBranches.setFont(serif12);

        // text box to enter min branch length
        JLabel kLabelMinBranchLength = new JLabel("Minimum branch length");

        kLabelMinBranchLength.setForeground(Color.black);
        kLabelMinBranchLength.setFont(serif12);
        m_kTextMinBranchLength = new JTextField(10);
        m_kTextMinBranchLength.setText(Float.toString(m_kOptions.m_fMinBranchLength));
        m_kTextMinBranchLength.setFont(serif12);

        // text box to enter the percentage of number of samples points to
        // use as BSpline control points.
        JLabel kLabelPercentBSplineNumControlPoints = new JLabel("BSpline fit num control points fraction of branch samples");

        kLabelPercentBSplineNumControlPoints.setForeground(Color.black);
        kLabelPercentBSplineNumControlPoints.setFont(serif12);
        m_kTextPercentBSplineNumControlPoints = new JTextField(10);
        m_kTextPercentBSplineNumControlPoints.setText(Float.toString(m_kOptions.m_fFractionNumControlPoints));
        m_kTextPercentBSplineNumControlPoints.setFont(serif12);

        // combo box to select path samples reduction factor to use when
        // segmenting the surface
        JLabel kLabelSegmentSurfaceBranchSamplesReductionFactor = new JLabel("Surface segmentation branch samples reduction factor");

        kLabelSegmentSurfaceBranchSamplesReductionFactor.setForeground(Color.black);
        kLabelSegmentSurfaceBranchSamplesReductionFactor.setFont(serif12);
        kLabelSegmentSurfaceBranchSamplesReductionFactor.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kComboSegmentSurfaceBranchSamplesReductionFactor = new JComboBox();
        m_kComboSegmentSurfaceBranchSamplesReductionFactor.setFont(MipavUtil.font12);
        m_kComboSegmentSurfaceBranchSamplesReductionFactor.setBackground(Color.white);
        m_kComboSegmentSurfaceBranchSamplesReductionFactor.addItem("1 (no reduction)");
        m_kComboSegmentSurfaceBranchSamplesReductionFactor.addItem("2");
        m_kComboSegmentSurfaceBranchSamplesReductionFactor.addItem("4");
        m_kComboSegmentSurfaceBranchSamplesReductionFactor.setSelectedIndex(1);
        
        // Button: Load Surface
        flythruButtonLoadImage.setText("Select Mask Image ...");
        flythruButtonLoadImage.setActionCommand("Select Image ...");
        flythruButtonLoadImage.addActionListener(this);

        // Label: name of loaded surface file
        flythruLabelFileName.setPreferredSize(new Dimension(130, 21));
        flythruLabelFileName.setBorder(BorderFactory.createLoweredBevelBorder());
        flythruLabelFileName.setText("<no image loaded>");

        JPanel kPanelOptions = new JPanel(new GridBagLayout());

        kPanelOptions.setBorder(buildTitledBorder("Image Options"));

        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        kPanelOptions.add(kLabelMaxNumBranches, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        kPanelOptions.add(m_kTextMaxNumBranches, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        kPanelOptions.add(kLabelMinBranchLength, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        kPanelOptions.add(m_kTextMinBranchLength, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        kPanelOptions.add(kLabelPercentBSplineNumControlPoints, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        kPanelOptions.add(m_kTextPercentBSplineNumControlPoints, gbc);

        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        kPanelOptions.add(kLabelSegmentSurfaceBranchSamplesReductionFactor, gbc);
        gbc.gridx = 0;
        gbc.gridy = 7;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        kPanelOptions.add(m_kComboSegmentSurfaceBranchSamplesReductionFactor, gbc);    
        
        JPanel fileLoaderPanel = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        fileLoaderPanel.add(flythruButtonLoadImage, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        fileLoaderPanel.add(flythruLabelFileName, gbc);

        gbc.gridx = 0;
        gbc.gridy = 8;
        kPanelOptions.add(fileLoaderPanel, gbc);

        contentBox.add(kPanelOptions);

        JPanel surfaceControlPanel = new JPanel();

        surfaceControlPanel.setLayout(new BoxLayout(surfaceControlPanel, BoxLayout.Y_AXIS));
        surfaceControlPanel.setBorder(buildTitledBorder("Surface Options"));

        JPanel updatePanel = new JPanel();

        updatePanel.setLayout(new GridBagLayout());
        gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        updatePanel.add(m_kContinueUpdate, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        updatePanel.add(updateButton, gbc);

        JPanel surfacePanel = new JPanel();

        surfacePanel.setLayout(new GridBagLayout());
        gbc.gridx = 1;
        gbc.gridy = 0;

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        surfacePanel.add(m_kLabelDistance, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        surfacePanel.add(m_kTextDistance, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        surfacePanel.add(m_kLabelPosition, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        surfacePanel.add(m_kTextPosition, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        surfacePanel.add(m_kLabelDirection, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        surfacePanel.add(m_kTextDirection, gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        surfacePanel.add(m_kLabelOrientation, gbc);
        gbc.gridx = 1;
        gbc.gridy = 6;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        surfacePanel.add(m_kTextOrientation, gbc);
        gbc.gridx = 0;
        gbc.gridy = 8;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        surfacePanel.add(m_kLabelBranch, gbc);
        gbc.gridx = 1;
        gbc.gridy = 8;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        surfacePanel.add(m_kTextBranch, gbc);
        gbc.gridx = 0;
        gbc.gridy = 10;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        surfacePanel.add(m_kLabelStepGaze, gbc);
        gbc.gridx = 1;
        gbc.gridy = 10;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        surfacePanel.add(m_kTextStepGaze, gbc);
        gbc.gridx = 0;
        gbc.gridy = 12;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        surfacePanel.add(m_kCheckBoxShowCurvatures, gbc);
        gbc.gridx = 1;
        gbc.gridy = 12;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        surfacePanel.add(updatePanel, gbc);

        surfaceControlPanel.add(surfacePanel);
        contentBox.add(surfaceControlPanel);

        kPanelButton = new JPanel();
        buildOKButton();
        OKButton.setText("Load");
        OKButton.setEnabled(false);
        kPanelButton.add(OKButton);
        contentBox.add(kPanelButton);

        // Labels: distance, position, direction, orientation, step
        m_kLabelDistance.setText("Distance");
        m_kLabelPosition.setText("Position");
        m_kLabelDirection.setText("Direction");
        m_kLabelOrientation.setText("Orientation");
        m_kLabelBranch.setText("Branch Info");
        m_kLabelStepGaze.setText("Step/Gaze Dist");

        // Text display: distance, position, direciton, orientation, step
        m_kTextDistance.setPreferredSize(new Dimension(175, 21));
        m_kTextDistance.setEditable(false);
        m_kTextPosition.setPreferredSize(new Dimension(175, 21));
        m_kTextPosition.setEditable(false);
        m_kTextDirection.setPreferredSize(new Dimension(175, 21));
        m_kTextDirection.setEditable(false);
        m_kTextOrientation.setPreferredSize(new Dimension(175, 21));
        m_kTextOrientation.setEditable(false);
        m_kTextBranch.setPreferredSize(new Dimension(175, 21));
        m_kTextBranch.setEditable(false);
        m_kTextStepGaze.setPreferredSize(new Dimension(175, 21));
        m_kTextStepGaze.setEditable(false);

        // Label: name of loaded surface file
        m_kLabelFileName.setPreferredSize(new Dimension(130, 21));
        m_kLabelFileName.setBorder(BorderFactory.createLoweredBevelBorder());
        m_kLabelFileName.setText("<no surface loaded>");

        // Check box: show mean curvatures
        m_kCheckBoxShowCurvatures.setText("Pseudocolored");
        m_kCheckBoxShowCurvatures.setSelected(m_bShowMeanCurvatures);
        m_kCheckBoxShowCurvatures.addActionListener(new java.awt.event.ActionListener() {
                @Override
				public void actionPerformed(ActionEvent e) {
                    m_kCheckBoxShowCurvatures_actionPerformed(e);
                }
            });
        m_kCheckBoxShowCurvatures.setEnabled(false);

        // build the continue update checkbox
        m_kContinueUpdate.setText("Update View");
        m_kContinueUpdate.setSelected(continueUpdate);
        m_kContinueUpdate.addActionListener(new java.awt.event.ActionListener() {
                @Override
				public void actionPerformed(ActionEvent e) {
                    m_kContinueUpdate_actionPerformed(e);
                }
            });

        updateButton.setIcon(MipavUtil.getIcon("moveslice.gif"));
        updateButton.setRolloverIcon(MipavUtil.getIcon("movesliceroll.gif"));
        updateButton.setBorderPainted(false);
        updateButton.setToolTipText("Update view position");

        updateButton.addActionListener(this);
        updateButton.setActionCommand("Update");
        updateButton.setFont(MipavUtil.font12B);
        updateButton.setMinimumSize(MipavUtil.defaultButtonSize);
        updateButton.setEnabled(false);
    }
    
    /**
     * Method to load the mask image.
     */
    private void loadingImage() {
        FileIO fileIO = new FileIO();
        fileIO.setQuiet(true);
        endoscopyImage = fileIO.readImage(fileName, directory, false, null);
        m_kView.setupRender(endoscopyImage, m_kOptions);
        m_kView.GetCanvas().display();
    }

    /**
	     * Sets up the variables needed for the algorithm from the GUI components.
	     * @return  Flag indicating if the setup was successful.
	     */
	    private boolean setVariables() {
	
	        String tmpStr;
	
	        // Maximum number of branches.
	        tmpStr = m_kTextMaxNumBranches.getText();
	
	        if (testParameterMin(tmpStr, 1)) {
	            m_kOptions.m_iMaxBranches = Integer.valueOf(tmpStr).intValue();
	        } else {
	            m_kTextMaxNumBranches.requestFocus();
	            m_kTextMaxNumBranches.selectAll();
	
	            return false;
	        }
	
	        // Minimum branch length.
	        tmpStr = m_kTextMinBranchLength.getText();
	
	        if (testParameterMin(tmpStr, 0.1)) {
	            m_kOptions.m_fMinBranchLength = Float.valueOf(tmpStr).floatValue();
	        } else {
	            m_kTextMinBranchLength.requestFocus();
	            m_kTextMinBranchLength.selectAll();
	
	            return false;
	        }
	
	        // BSpline number of control points fraction of
	        // number of branch path samples.
	        tmpStr = m_kTextPercentBSplineNumControlPoints.getText();
	
	        if (testParameter(tmpStr, 0.01, 0.99)) {
	            m_kOptions.m_fFractionNumControlPoints = Float.valueOf(tmpStr).floatValue();
	        } else {
	            m_kTextPercentBSplineNumControlPoints.requestFocus();
	            m_kTextPercentBSplineNumControlPoints.selectAll();
	
	            return false;
	        }
	        
	        // Path samples reduction factor for segmentation of the surface.
	        m_kOptions.m_iSegmentSurfaceBranchSamplesReductionFactor = 1 <<
	        m_kComboSegmentSurfaceBranchSamplesReductionFactor.getSelectedIndex();
	       
	        return true;
	    }

    /**
     * Update the display to show the current branch and path direction (forward or reverse) for moving along the length
     * of the path.
     *
     * @param  iBranch           index which identifies the branch currently on
     * @param  bPathMoveForward  flag set if moving forward, as opposed to in reverse, along the length of the path.
     */
    protected void setBranchInfo(int iBranch, boolean bPathMoveForward) {
        m_kTextBranch.setText((bPathMoveForward ? "Start-to-End" : "End-to-Start") + " on " +
                              Integer.toString(iBranch));
    }

    /**
     * Update the display to show the current distance along the length of the path.
     * @param  fDist       distance along the path from the start
     * @param  fTotalDist  total distance along the path from start to end
     */
    protected void setPathDistance(float fDist, float fTotalDist) {
        m_kDecimalFormat.setMinimumFractionDigits(1);
        m_kDecimalFormat.setMaximumFractionDigits(1);
        m_kTextDistance.setText(m_kDecimalFormat.format(fDist) + " of " + m_kDecimalFormat.format(fTotalDist) + "  (" +
                                m_kDecimalFormat.format(100.0f * fDist / fTotalDist) + "%)");
    }

    /**
     * Update the display to show the coordinates of the current position along the path.
     * @param  kPosition  3D coordinates of current position
     */
    protected void setPathPosition(Vector3f kPosition) {
        m_kDecimalFormat.setMinimumFractionDigits(1);
        m_kDecimalFormat.setMaximumFractionDigits(1);
        m_kTextPosition.setText(m_kDecimalFormat.format(kPosition.X) + " " + m_kDecimalFormat.format(kPosition.Y) +
                                " " + m_kDecimalFormat.format(kPosition.Z));
    }

    /**
     * Update the display to show the current gaze distance for looking ahead down the path.
     * @param  fStepDist  magnitude is the distance increment along the path for moving; sign is the direction of moving
     *                    along the path from one end to the other or vice versa.
     * @param  fGazeDist  distance ahead for looking down the path.
     */
    protected void setStepGazeDist(float fStepDist, float fGazeDist) {
        m_kTextStepGaze.setText(m_kDecimalFormat.format(Math.abs(fStepDist)) + " / " +
                                m_kDecimalFormat.format(fGazeDist));
    }

    /**
     * Update the display to show the current base viewing direction at the current point along the path.
     * @param  kVector  normalized direciton vector
     */
    protected void setViewDirection(Vector3f kVector) {
        m_kDecimalFormat.setMinimumFractionDigits(2);
        m_kDecimalFormat.setMaximumFractionDigits(2);
        m_kTextDirection.setText(m_kDecimalFormat.format(kVector.X) + " " + m_kDecimalFormat.format(kVector.Y) + " " +
                                 m_kDecimalFormat.format(kVector.Z));
    }

    /**
     * Update the display to show the current viewing orientation which is always relative to the base viewing
     * direction.
     * @param  kMatrix  3x3 matrix containing orientation transform
     */
    protected void setViewOrientation(Matrix3f kMatrix) {
        m_kDecimalFormat.setMinimumFractionDigits(1);
        m_kDecimalFormat.setMaximumFractionDigits(1);

        float fRotateX = (float) Math.toDegrees(Math.atan2((-kMatrix.M12), (kMatrix.M22)));
        float fRotateY = (float) Math.toDegrees(Math.asin(((-kMatrix.M02))));
        float fRotateZ = (float) Math.toDegrees(Math.atan2((-kMatrix.M01), (kMatrix.M00)));

        m_kTextOrientation.setText(m_kDecimalFormat.format(fRotateX) + " " + m_kDecimalFormat.format(fRotateY) + " " +
                                   m_kDecimalFormat.format(fRotateZ));
    }

    /**
     * Tests that the entered parameter is larger than the specified value.
     * @param   str       The value entered by the user.
     * @param   minValue  The minimum value this variable may be set to.
     * @return  <code>true</code> if parameters passed range test, <code>false</code> if failed.
     */
    protected boolean testParameterMin(String str, double minValue) {
        double tmp;

        try {
            tmp = Double.valueOf(str).doubleValue();

            if (tmp < minValue) {
                MipavUtil.displayError("Value is smaller than " + String.valueOf(minValue));

                return false;
            } 
            return true;
        } catch (NumberFormatException error) {
            MipavUtil.displayError("Must enter numeric value");

            return false;
        }
    } 
    
    /**
     * Show the psesudo color.
     * @param  e  ActionEvent
     */
    void m_kCheckBoxShowCurvatures_actionPerformed(@SuppressWarnings("unused")
    ActionEvent e) {
        m_bShowMeanCurvatures = m_kCheckBoxShowCurvatures.isSelected();
        m_kView.doPseudoColor(m_bShowMeanCurvatures);
    }

 /**
 * Continue update the surface render and the plane render.
 * @param  e  ActionEvent
 */
void m_kContinueUpdate_actionPerformed(@SuppressWarnings("unused")
ActionEvent e) {
    continueUpdate = m_kContinueUpdate.isSelected();

    if (continueUpdate == true) {
        updateButton.setEnabled(false);
    } else {
        updateButton.setEnabled(true);
    }
}

}
