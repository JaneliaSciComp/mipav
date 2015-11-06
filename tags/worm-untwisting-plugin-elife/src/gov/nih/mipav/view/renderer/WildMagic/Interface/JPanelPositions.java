package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;

import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.renderer.JPanelRendererBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;


import java.awt.Color;

import java.awt.Dimension;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;



import javax.swing.ButtonGroup;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;





import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 */
public class JPanelPositions extends JInterfaceBase {

    /**  */
    private static final long serialVersionUID = 4578542603991193148L;

    /** Labels for the current position in 3D ModelView coordinates. */
    protected JLabel modelViewLabel = null;

    /** Displayed values for the current position in 3D ModelView coordinates. */
    protected JLabel[] modelViewLabelVals = new JLabel[3];

    /** Labels for the current position in PatientSlice coordinates. */
    protected JLabel patientSliceLabel = null;

    /** Displayed values for the current position in PatientSlice coordinates. */
    protected JLabel[] patientSliceLabelVals = new JLabel[3];

    /** Labels for the current absolute position:. */
    protected JLabel absoluteLabel = null;

    /** Labels for the current absolute position values:. */
    protected JLabel[] absoluteLabelVals = null;

    /** JPanel containing the absoulte position labels:. */
    protected JPanel absolutePanel = new JPanel(new GridBagLayout());
    

    /** Labels for the current scanner position:. */
    protected JLabel scannerLabel = null;

    /** Labels for the current scanner position values:. */
    protected JLabel[] scannerLabelVals = null;

    /** JPanel containing the scanner position labels:. */
    protected JPanel scannerPanel = new JPanel(new GridBagLayout());

    /** Panel containing the position labels:. */
    private JPanel panelLabels = new JPanel();

    private JRadioButton radiologicalView;
    private JRadioButton neurologicalView;
    
    private transient ModelImage imageA;
    
    /**
     * 3D clipping dialog control.
     * @param kVolumeViewer parent frame.
     */
    public JPanelPositions(VolumeTriPlanarInterface kVolumeViewer) {
        super(kVolumeViewer);
        imageA = kVolumeViewer.getImageA();
       
        init();
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        if (command.equals("RadiologicalView"))
        {
            m_kVolumeViewer.setRadiological(true);
        }
        else if (command.equals("NeurologicalView"))
        {
            m_kVolumeViewer.setRadiological(false);
        }
    }

    /**
     * Dispose memory.
     */
    public void disposeLocal() {
    }

    /**
     * Initializes GUI components.
     */
    public void init() {
        scannerLabel = new JLabel("Scanner Position");
        scannerLabel.setForeground(Color.black);
        scannerLabel.setFont(MipavUtil.font14B);
        scannerLabelVals = new JLabel[3];
        scannerLabelVals[0] = new JLabel("A-P: ");
        scannerLabelVals[1] = new JLabel("L-R: ");
        scannerLabelVals[2] = new JLabel("S-I: ");

        absoluteLabel = new JLabel("Absolute Volume coordinates");
        absoluteLabel.setToolTipText("Coordinates in 3D image space");
        absoluteLabel.setForeground(Color.black);
        absoluteLabel.setFont(MipavUtil.font14B);
        absoluteLabelVals = new JLabel[4];
        absoluteLabelVals[0] = new JLabel("X: ");
        absoluteLabelVals[1] = new JLabel("Y: ");
        absoluteLabelVals[2] = new JLabel("Z: ");
        absoluteLabelVals[3] = new JLabel("1D index: ");

        for (int i = 0; i < 3; i++) {
            scannerLabelVals[i].setForeground(Color.black);
            scannerLabelVals[i].setFont(MipavUtil.font12B);

            absoluteLabelVals[i].setForeground(Color.black);
            absoluteLabelVals[i].setFont(MipavUtil.font12B);
        }

        absoluteLabelVals[3].setForeground(Color.black);
        absoluteLabelVals[3].setFont(MipavUtil.font12B);

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.WEST;

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;

        scannerPanel.add(scannerLabel, gbc2);
        absolutePanel.add(absoluteLabel, gbc2);

        gbc2.gridy++;
        scannerPanel.add(new JLabel(), gbc2);
        absolutePanel.add(new JLabel(), gbc2);

        for (int i = 0; i < 3; i++) {
            gbc2.gridy++;
            scannerPanel.add(scannerLabelVals[i], gbc2);
            absolutePanel.add(absoluteLabelVals[i], gbc2);
        }

        gbc2.gridy++;
        absolutePanel.add(absoluteLabelVals[3], gbc2);
        
        
        patientSliceLabel = new JLabel("Patient Slice Position");
        patientSliceLabel.setForeground(Color.black);
        patientSliceLabel.setFont(MipavUtil.font14B);
        patientSliceLabelVals[0] = new JLabel("sagittal slice: ");
        patientSliceLabelVals[1] = new JLabel("coronal slice: ");
        patientSliceLabelVals[2] = new JLabel("axial slice: ");

        modelViewLabel = new JLabel("3D Model Position");
        modelViewLabel.setForeground(Color.black);
        modelViewLabel.setFont(MipavUtil.font14B);
        modelViewLabelVals[0] = new JLabel("X: ");
        modelViewLabelVals[1] = new JLabel("Y: ");
        modelViewLabelVals[2] = new JLabel("Z: ");

        for (int i = 0; i < 3; i++) {
            patientSliceLabelVals[i].setForeground(Color.black);
            patientSliceLabelVals[i].setFont(MipavUtil.font12B);

            modelViewLabelVals[i].setForeground(Color.black);
            modelViewLabelVals[i].setFont(MipavUtil.font12B);
        }

        JPanel patientSlicePanel = new JPanel(new GridBagLayout());
        JPanel modelViewPanel = new JPanel(new GridBagLayout());

        gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.WEST;

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;

        patientSlicePanel.add(patientSliceLabel, gbc2);
        modelViewPanel.add(modelViewLabel, gbc2);

        gbc2.gridy++;
        patientSlicePanel.add(new JLabel(), gbc2);
        modelViewPanel.add(new JLabel(), gbc2);

        for (int i = 0; i < 3; i++) {
            gbc2.gridy++;
            patientSlicePanel.add(patientSliceLabelVals[i], gbc2);
            modelViewPanel.add(modelViewLabelVals[i], gbc2);
        }

        radiologicalView = new JRadioButton("Radiological View");
        radiologicalView.setSelected(true);
        radiologicalView.addActionListener(this);
        radiologicalView.setActionCommand("RadiologicalView");

        neurologicalView = new JRadioButton("Neurological View");
        neurologicalView.setSelected(false);
        neurologicalView.addActionListener(this);
        neurologicalView.setActionCommand("NeurologicalView");

        ButtonGroup dataViewGroup = new ButtonGroup();
        dataViewGroup.add(radiologicalView);
        dataViewGroup.add(neurologicalView);


        JPanel viewPanel = new JPanel(new GridBagLayout());
        gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.CENTER;
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        viewPanel.setBorder(JPanelRendererBase.buildTitledBorder("Viewing Convention"));
        viewPanel.add(radiologicalView, gbc2);
        gbc2.gridy = 1;
        viewPanel.add(neurologicalView, gbc2);

        JPanel panelLabelsModel = new JPanel();
        panelLabelsModel.setLayout(new GridLayout(1, 2));
        panelLabelsModel.setBorder(JPanelRendererBase.buildTitledBorder("Rendering Coordinates"));
        panelLabelsModel.add(modelViewPanel);
        panelLabelsModel.add(patientSlicePanel);

        JPanel panelLabelsScanner = new JPanel();
        panelLabelsScanner.setLayout(new GridLayout(1, 2));
        panelLabelsScanner.setBorder(JPanelRendererBase.buildTitledBorder("Scanner Coordinates"));
        panelLabelsScanner.add(scannerPanel);
        panelLabelsScanner.add(absolutePanel);

        panelLabels.setLayout(new GridLayout(3, 1));
        panelLabels.add(panelLabelsScanner);
        panelLabels.add(viewPanel);
        panelLabels.add(panelLabelsModel);
    }

    public JPanel getMainPanel()
    {
        return panelLabels;
    }  
    
    public void setRadiological( boolean bOn )
    {
        radiologicalView.setSelected(bOn);
        neurologicalView.setSelected(!bOn);
    }
    
    /**
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
    *
    * @param  panelWidth   width
    * @param  frameHeight  height
    */
   public void resizePanel(int panelWidth, int frameHeight) {
       panelLabels.setPreferredSize(new Dimension(panelWidth, frameHeight - 40));
       panelLabels.setSize(new Dimension(panelWidth, frameHeight - 40));
       panelLabels.revalidate();
   }
    
    /**
     * Sets the position labels.
     *
     * @param  position  the slice positions in FileCoordinates.
     */
    public void setPositionLabels(Vector3f position) {

        if (scannerLabel == null) {
            return;
        }

        setScannerPosition(position);
        setPatientSlicePosition(position);
        set3DModelPosition(position);
        setAbsPositionLabels(position);
    }    


    /**
     * Sets the 3DModel position label.
     * @param  position  3DModel position values.
     */
    protected void set3DModelPosition(Vector3f position) {

        float fMaxX = (imageA.getExtents()[0] - 1) * imageA.getFileInfo(0).getResolutions()[0];
        float fMaxY = (imageA.getExtents()[1] - 1) * imageA.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (imageA.getExtents()[2] - 1) * imageA.getFileInfo(0).getResolutions()[2];

        float fMax = fMaxX;
        if (fMaxY > fMax) {
            fMax = fMaxY;
        }
        if (fMaxZ > fMax) {
            fMax = fMaxZ;
        }
        float fX = fMaxX/fMax;
        float fY = fMaxY/fMax;
        float fZ = fMaxZ/fMax;

        fX *= position.X/(imageA.getExtents()[0] - 1);
        fY *= position.Y/(imageA.getExtents()[1] - 1);
        fZ *= position.Z/(imageA.getExtents()[2] - 1);

        modelViewLabelVals[0].setText("X: " + fX);
        modelViewLabelVals[1].setText("Y: " + fY);
        modelViewLabelVals[2].setText("Z: " + fZ);

    }
    
    /**
     * Sets the PatientSlice position label.
     * @param  position value.
     */
    protected void setPatientSlicePosition(Vector3f position) {
        Vector3f axial = new Vector3f();
        MipavCoordinateSystems.fileToPatient(position, axial, imageA, FileInfoBase.AXIAL);

        Vector3f coronal = new Vector3f();
        MipavCoordinateSystems.fileToPatient(position, coronal, imageA, FileInfoBase.CORONAL);

        Vector3f sagittal = new Vector3f();
        MipavCoordinateSystems.fileToPatient(position, sagittal, imageA, FileInfoBase.SAGITTAL);

        patientSliceLabelVals[0].setText("sagittal slice: " + (int) sagittal.Z);
        patientSliceLabelVals[1].setText("coronal slice: " + (int) coronal.Z);
        patientSliceLabelVals[2].setText("axial slice: " + (int) axial.Z);
    }


    /**
     * Sets the Absolute position label.
     *
     * @param  position  DOCUMENT ME!
     */
    protected void setAbsPositionLabels(Vector3f position) {

        if (absoluteLabelVals == null) {
            return;
        }

        absoluteLabelVals[0].setText("X: " + (int) position.X);
        absoluteLabelVals[1].setText("Y: " + (int) position.Y);
        absoluteLabelVals[2].setText("Z: " + (int) position.Z);

        int[] dimExtents = imageA.getExtents();
        int index = (int) ((position.Z * dimExtents[0] * dimExtents[1]) + (position.Y * dimExtents[0]) + position.X);

        int iBuffFactor = imageA.isColorImage() ? 4 : 1;
        if ( (index*iBuffFactor > imageA.getSize()) || (index < 0) )
        {
            return;
        }
        absoluteLabelVals[3].setText("1D index: " + index + " = " + imageA.getFloat(index * iBuffFactor));
    }

    /**
     * Sets the Scanner position label.
     *
     * @param  position  DOCUMENT ME!
     */
    protected void setScannerPosition(Vector3f position) {

        if (scannerLabelVals == null) {
            return;
        }

        String[] labelContents = ViewJComponentEditImage.getScannerPositionLabels(imageA, position);

        for (int i = 0; i < labelContents.length; i++) {
            scannerLabelVals[i].setText(labelContents[i]);
        }
    }

}
