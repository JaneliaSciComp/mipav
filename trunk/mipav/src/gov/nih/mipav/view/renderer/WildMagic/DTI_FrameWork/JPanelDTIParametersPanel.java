package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;


import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;


import gov.nih.mipav.view.renderer.WildMagic.*;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;

import java.awt.*;
import java.awt.event.*;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import java.util.*;
import javax.swing.*;
import javax.swing.event.*;

public class JPanelDTIParametersPanel extends JInterfaceBase 
implements ItemListener, ListSelectionListener, ChangeListener {
    /**
     * 
     */
    private static final long serialVersionUID = 2778749302064237729L;

    /** Box layout for control panel. */
    private Box contentBox;

    private JCheckBox reconstructTracts;
    private JButton computeButton;

    /** The list box in the dialog for fiber bundle tracts. */
    private JList m_kTractList;
    /** Color button for changing the color of the fiber bundles. */
    private JButton m_kColorButton;
    /** Color button detault color: */
    private Color m_kColorButtonDefault;
    /** Checkbox for turning on/off volume color for the polylines. */
    private JCheckBox m_kUseVolumeColor;
    /** Checkbox for switching between polylines and ellipsoids. */
    private JCheckBox m_kUseEllipsoids;
    /** Checkbox for displaying all tensors as ellipsoids. */
    private JCheckBox m_kAllEllipsoids;
    /** Checkbox for switching between polylines and ellipsoids and cylinders. */
    private JCheckBox m_kUseCylinders;
    /** Checkbox for switching between polylines and ellipsoids and cylinders, Tubes */
    private JCheckBox m_kTubes;
    /** Checkbox for displaying all tensors as ellipsoids. */
    private JCheckBox m_kAllCylinders;
    /** User-control over the number of ellipsoids displayed in GPUVolumeRender */
    private JSlider m_kDisplaySlider;

    private VolumeTriPlanarInterfaceDTI parentFrame;

    private VolumeTriPlanarRender m_kVolumeDisplay;

    private ModelImage m_kDTIImage;

    /** Tract input file. */
    private File m_kTractFile = null;

    /** For TRACTS dialog: number of tracts to display. */
    private JTextField m_kTractsLimit;

    /** For TRACTS dialog: minimum tract length to display. */
    private JTextField m_kTractsMin;

    /** For TRACTS dialog: maximum tract length to display. */
    private JTextField m_kTractsMax;


    /** When selected, only tracts that intersect the VOI are displayed. */
    private JCheckBox m_kUseVOICheck = null;

    /** Keeps track of the groups of polylines loaded. */
    private Vector<Integer> m_kBundleList = new Vector<Integer>();

    /** Number of currently loaded fiber bundle groups. */
    private int m_iBundleCount = 0;

    private ModelImage m_kImage;

    /** Fiber bundle tract file input path name text box. */
    private JTextField m_kTractPath;

    /** Which tensor nodes are already on the fiber bundle tract */
    private boolean[] m_abVisited = null;

    /** Check boxe enable the slice pickable or not */
    public JCheckBox slicePickableCheckBox;

    private JRadioButton radioLines;
    private JRadioButton radioEllipzoids;
    private JRadioButton radioCylinders;
    private JRadioButton radioTubes;
    private JRadioButton radioArrows;
    private JCheckBox displayAllCheckBox;
    private int displayMode;
    private static int Polylines = 0;
    private static int Ellipzoids = 1;
    private static int Tubes = 2;
    private static int Cylinders = 3;
    private static int Arrows = 4;
    private boolean displayAll;
    private int centerIndex;
    private boolean loadingTrack = false;

    public JPanelDTIParametersPanel(VolumeTriPlanarInterfaceDTI _parentFrame, VolumeTriPlanarRender _m_kVolumeDisplay) {
        parentFrame = _parentFrame;
        m_kVolumeDisplay = _m_kVolumeDisplay;
        mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.add(createTractPanel());
        createTractDialog();

    }

    /**
     * Color chooser for when the user wants to change the color of the fiber
     * bundle tracts.
     */
    private ViewJColorChooser m_kColorChooser;



    /**
     * @param  e  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == m_kDisplaySlider) {
            // m_kVolumeDisplay.setEllipseMod( m_kDisplaySlider.getValue() );
        }
    }

    public void valueChanged(ListSelectionEvent kEvent) {

    }

    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();
        if (command.equals("ChangeColor")) {
            m_kColorChooser = new ViewJColorChooser(new Frame(),
                    "Pick fiber bundle color", new OkColorListener(),
                    new CancelListener());
        } else if (command.equals("VolumeColor")) {
            if (m_kUseVolumeColor.isSelected()) {
                setColor(null);
            } else {
                setColor(m_kColorButton.getBackground());
            }
        } else if ( command.equals("DisplayAll") ) {
            displayAll = displayAllCheckBox.isSelected();
            invokeDisplayFunction();
        } else if (command.equals("Add")) {
            loadingTrack = true;
            loadTractFile();
            loadingTrack = false;
            slicePickableCheckBox.setEnabled(true);
        } else if (command.equals("Remove")) {
            removePolyline();
        } else if( command.equals("Pickable")) {
            ((VolumeTriPlanerRenderDTI)m_kVolumeDisplay).enableSlicePickable(slicePickableCheckBox.isSelected());	
        }

    }

    public void updateCounter() {
        if ( slicePickableCheckBox.isSelected() ) {
            updateTractCount();
        } 
    }


    public void addFiberTract() {
        if ( slicePickableCheckBox.isSelected() ) {
            addTract();
        } 
    }

    /**
     * Creates the user-interface for the Fiber Bundle Tract dialog.
     * 
     * @return JPanel containing the user-interface for the Fiber Bundle Tract
     *         dialog.
     */
    private JPanel createTractDialog() {
        GridBagLayout kGBL = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.anchor = GridBagConstraints.NORTHWEST;

        JPanel kTractPanel = new JPanel(kGBL);

        JPanel kParamsPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        JLabel kNumberTractsLimit = new JLabel(
        "Maximum number of tracts to display:");
        kParamsPanel.add(kNumberTractsLimit, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        m_kTractsLimit = new JTextField("100", 5);
        m_kTractsLimit.setBackground(Color.white);
        kParamsPanel.add(m_kTractsLimit, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        JLabel m_kTractsMinLength = new JLabel("Minimum tract length:");
        kParamsPanel.add(m_kTractsMinLength, gbc);
        gbc.gridx++;
        m_kTractsMin = new JTextField("50", 5);
        m_kTractsMin.setBackground(Color.white);
        kParamsPanel.add(m_kTractsMin, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        JLabel m_kTractsMaxLength = new JLabel("Maximum tract length:");
        kParamsPanel.add(m_kTractsMaxLength, gbc);
        gbc.gridx++;
        m_kTractsMax = new JTextField("100", 5);
        m_kTractsMax.setBackground(Color.white);
        kParamsPanel.add(m_kTractsMax, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        JLabel kUseVOI = new JLabel("Use VOI:");
        kParamsPanel.add(kUseVOI, gbc);
        gbc.gridx++;
        m_kUseVOICheck = new JCheckBox("use voi");
        kParamsPanel.add(m_kUseVOICheck, gbc);

        JPanel filesPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        JLabel kTractLabel = new JLabel(" DTI tract file: ");
        filesPanel.add(kTractLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        m_kTractPath = new JTextField(35);
        m_kTractPath.setEditable(true);
        m_kTractPath.setBackground(Color.white);
        filesPanel.add(m_kTractPath, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.weightx = 0;
        JButton kTractBrowseButton = new JButton("Browse");
        kTractBrowseButton.addActionListener(this);
        kTractBrowseButton.setActionCommand("tractBrowse");
        filesPanel.add(kTractBrowseButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        kTractPanel.add(kParamsPanel, gbc);
        gbc.gridy++;
        kTractPanel.add(filesPanel, gbc);

        return kTractPanel;
    }


    /**
     * Launches the JFileChooser for the user to select the tract file. Stores
     * the File for the tract file but does not read the file.
     */
    private void loadTractFile() {
        JFileChooser chooser = new JFileChooser(new File(Preferences
                .getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(
                ViewImageFileFilter.ALL));
        chooser.setDialogTitle("Choose Diffusion Tensor Tract file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            String kDTIName = new String(chooser.getSelectedFile().getName());
            String kTract = new String("_tract");
            kDTIName = kDTIName.substring(0, kDTIName.length()
                    - kTract.length());
            // Why disable the check?  Left later.  
            // if (parentFrame.getDTIimage() == null) {

            FileIO fileIO = new FileIO();
            m_kDTIImage = fileIO.readImage(kDTIName, chooser
                    .getCurrentDirectory()
                    + File.separator);
            if (m_kDTIImage.getNDims() != 4) {
                MipavUtil
                .displayError("Diffusion Tensor file does not have correct dimensions");
                if (m_kDTIImage != null) {
                    m_kDTIImage.disposeLocal();
                }
                m_kDTIImage = null;
            }
            if (m_kDTIImage.getExtents()[3] != 6) {
                MipavUtil
                .displayError("Diffusion Tensor does not have correct dimensions");
                if (m_kDTIImage != null) {
                    m_kDTIImage.disposeLocal();
                }
                m_kDTIImage = null;
            }

            setDTIImage(m_kDTIImage);
            // }

            m_kTractFile = new File(chooser.getSelectedFile().getAbsolutePath());
            if (!m_kTractFile.exists() || !m_kTractFile.canRead()) {
                m_kTractFile = null;
                return;
            }
            int iLength = (int) m_kTractFile.length();
            if (iLength <= 0) {
                m_kTractFile = null;
                return;
            }
            // m_kTractPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser
                    .getCurrentDirectory().toString());
            processTractFile();
        }
    }

    /**
     * Pass the DTI image to the GPUVolumeRender.
     * 
     * @param kDTIImage
     *            new DTI image.
     */
    protected void setDTIImage(ModelImage kDTIImage) {
        m_kDTIImage = kDTIImage;
        m_kVolumeDisplay.setDTIImage(m_kDTIImage);
        m_kVolumeDisplay.setEllipseMod(m_kDisplaySlider.getValue());
    }

    /** Updates the number of fiber bundle tract groups. */
    protected void updateTractCount() {
        m_iBundleCount = getMinUnused(m_kBundleList);
    }

    /**
     * Gets a new fiber bundle index.
     * 
     * @param kBundleList
     *            list of fiber bundles.
     */
    private int getMinUnused(Vector<Integer> kBundleList) {
        int iMin = 0;
        if (kBundleList.size() == 0) {
            return iMin;
        }
        boolean bFound = false;
        /*
		for (int i = 0; i < kBundleList.size(); i++) {
			iMin = i;
			bFound = false;
			for (int j = 0; j < kBundleList.size(); j++) {
				if (iMin == kBundleList.get(j).intValue()) {
					bFound = true;
				}
			}
			if (!bFound) {
				return iMin;
			}
		}
         */
        for (int i = 0; i < kBundleList.size(); i++) {
            iMin = kBundleList.get(i).intValue();
        }
        iMin++;
        return iMin;
    }




    /**
     * process the tract file. Uses the File stored from the loadTractFile fn.
     * Loads fiber bundle tracts, filters them with the user-defined display
     * parameters, and passes them to the GPUVolumeRender for display.
     */
    private void processTractFile() {
        if (m_kTractFile == null) {
            MipavUtil.displayError("Tract file must be set.");
            return;
        }

        try {
            updateTractCount();

            boolean bTractsAdded = false;

            int iNumTractsLimit = (new Integer(m_kTractsLimit.getText()))
            .intValue();
            int iTractMinLength = (new Integer(m_kTractsMin.getText()))
            .intValue();
            int iTractMaxLength = (new Integer(m_kTractsMax.getText()))
            .intValue();

            int iNumTracts = 0;

            ModelImage kVOIImage = null;
            if (m_kUseVOICheck.isSelected()) {
                kVOIImage = ViewUserInterface.getReference()
                .getActiveImageFrame().getActiveImage()
                .generateBinaryImage(false, true);
            }

            int iDimX = 0, iDimY = 0, iDimZ = 0;
            FileInputStream kFileReader = new FileInputStream(m_kTractFile);
            int iBufferSize = 3 * 4;

            byte[] racBuffer = new byte[iBufferSize];
            kFileReader.read(racBuffer, 0, iBufferSize);
            ByteArrayInputStream acBufferIn = new ByteArrayInputStream(
                    racBuffer);
            DataInputStream acDataIn = new DataInputStream(acBufferIn);
            try {
                iDimX = acDataIn.readInt();
                iDimY = acDataIn.readInt();
                iDimZ = acDataIn.readInt();
            } catch (IOException e) {
                e.printStackTrace();
            }
            acBufferIn = null;
            acDataIn = null;
            racBuffer = null;

            int iLength = (int) m_kTractFile.length();
            int iBufferNext = iBufferSize;
            while (iBufferNext < iLength) {
                if (iNumTracts >= iNumTractsLimit) {
                    break;
                }

                Vector<Integer> kTract = inputTract(kFileReader);
                iBufferNext += kTract.size() * 4 + 4;
                int iVQuantity = kTract.size();
                if (contains(kVOIImage, kTract)) {
                    if ((iVQuantity > iTractMinLength)
                            && (iVQuantity < iTractMaxLength)) {
                        if (iNumTracts < iNumTractsLimit) {
                            iNumTracts++;
                            bTractsAdded = true;
                            addTract(kTract, iVQuantity, iDimX, iDimY, iDimZ);
                        }
                    }
                }
            }
            if (bTractsAdded) {
                addTract();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


    /** Updates the tract list user-interface. */
    public void addTract() {
        m_kBundleList.add(new Integer(m_iBundleCount));

        DefaultListModel kList = (DefaultListModel) m_kTractList.getModel();
        int iSize = kList.getSize();
        kList.add(iSize, new String("FiberBundle" + m_iBundleCount));
        m_kTractList.setSelectedIndex(iSize);
    }

    /** Adds a fiber bundle tract to the GPUVolumeRender and JPanelSurface.
     * @param kTract list of voxels in the fiber bundle.
     * @param iVQuantity number of voxels in the fiber bundle.
     * @param iDimX the x-dimensions of the DTI image used to create the tract.
     * @param iDimY the y-dimensions of the DTI image used to create the tract.
     * @param iDimZ the z-dimensions of the DTI image used to create the tract.
     */
    protected void addTract( Vector<Integer> kTract, int iVQuantity, int iDimX, int iDimY, int iDimZ )
    {
        m_kImage = parentFrame.getImageA();
        int iXBound = m_kImage.getExtents()[0];
        int iYBound = m_kImage.getExtents()[1];
        int iZBound = m_kImage.getExtents()[2];
        float fMaxX = (float) (iXBound - 1) * m_kImage.getFileInfo(0).getResolutions()[0];
        float fMaxY = (float) (iYBound - 1) * m_kImage.getFileInfo(0).getResolutions()[1];
        float fMaxZ = (float) (iZBound - 1) * m_kImage.getFileInfo(0).getResolutions()[2];

        float fMax = fMaxX;
        if (fMaxY > fMax) {
            fMax = fMaxY;
        }
        if (fMaxZ > fMax) {
            fMax = fMaxZ;
        }
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0,3);
        kAttr.SetCChannels(1,3);
        VertexBuffer pkVBuffer = new VertexBuffer(kAttr,iVQuantity);                        

        int iTractCount = 0;
        try {
            float fR = 0, fG = 0, fB = 0;

            for (int i = 0; i < iVQuantity; i++)
            {
                int iIndex = kTract.get(i);

                int iX = iIndex % iDimX;
                iIndex -= iX;
                iIndex /= iDimX;

                int iY = iIndex % iDimY;
                iIndex -= iY;
                iIndex /= iDimY;

                int iZ = iIndex;

                iIndex = kTract.get(i);

                if ( loadingTrack == true && i == (int)(iVQuantity/2) ) {
                    centerIndex = iIndex;
                }
                ColorRGB kColor1;
                if ( m_kImage.isColorImage() )
                {
                    fR = m_kImage.getFloat( iIndex*4 + 1 )/255.0f;
                    fG = m_kImage.getFloat( iIndex*4 + 2 )/255.0f;
                    fB = m_kImage.getFloat( iIndex*4 + 3 )/255.0f;
                    kColor1 = new ColorRGB(fR, fG, fB);
                }
                else
                {
                    fR = m_kImage.getFloat( iIndex );
                    kColor1 = new ColorRGB(fR, fR, fR);
                }

                float fX = (float)(iX)/(float)(iDimX);
                float fY = (float)(iY)/(float)(iDimY);
                float fZ = (float)(iZ)/(float)(iDimZ);

                pkVBuffer.SetPosition3(i,
                        (float)(fX-.5f), (float)(fY-.5f), (float)(fZ-.5f) );
                pkVBuffer.SetColor3(0,i, new ColorRGB(fX, fY, fZ));
                pkVBuffer.SetColor3(1,i, kColor1 );
                iTractCount++;

            }
        } catch ( Exception e ) {
            return;
        }
        boolean bClosed = false;
        boolean bContiguous = true;
        // addPolyline( new Polyline(pkVBuffer,bClosed,bContiguous) );
        // apply B-spline filter to smooth the track
        if ( iVQuantity >= 7 ) {
            addPolyline(new Polyline(smoothTrack(pkVBuffer, kTract,iVQuantity, iDimX, iDimY, iDimZ), bClosed, bContiguous ));
            m_iBundleCount++;
        }
    }

    /**
     * Smooth the fiber tracks with B-spline interpolation
     * 
     * @param pkVBuffer
     *            fiber track vertex coordinates as the control points.
     * @param kTract
     *            fiber track index list.
     * @param iVQuantity
     *            number of voxels in the fiber bundle.
     * @return  B-spline interpolated fiber track
     */
    private VertexBuffer smoothTrack(VertexBuffer pkVBuffer, Vector<Integer> kTract, int iVQuantity, int iDimX, int iDimY, int iDimZ ) {
        float fX_0, fY_0, fZ_0;
        float fX_1, fY_1, fZ_1;
        float fX_2, fY_2, fZ_2;
        float fX_3, fY_3, fZ_3;

        // curve sub-division number, default to 10.  
        int curveSubD = 1;
        float u, u_2, u_3;

        Attributes attr = new Attributes();
        attr.SetPChannels(3);
        attr.SetCChannels(0, 3);
        attr.SetCChannels(1, 3);
        VertexBuffer bsplineVBuffer = new VertexBuffer(attr, (iVQuantity -3) * curveSubD);		

        int index = 0;

        float fR = 0, fG = 0, fB = 0;

        float pos_x, pos_y, pos_z;

        for (int i = 0; i < iVQuantity-3; i++) {
            for(int j = 0; j < curveSubD; j++) {

                ColorRGB resultUnit0, resultUnit1;		

                u = (float)j / curveSubD;
                u_2 = u * u;
                u_3 = u_2 * u;

                fX_0 = pkVBuffer.GetPosition3fX(i);
                fY_0 = pkVBuffer.GetPosition3fY(i);
                fZ_0 = pkVBuffer.GetPosition3fZ(i);

                fX_1 = pkVBuffer.GetPosition3fX(i+1);
                fY_1 = pkVBuffer.GetPosition3fY(i+1);
                fZ_1 = pkVBuffer.GetPosition3fZ(i+1);

                fX_2 = pkVBuffer.GetPosition3fX(i+2);
                fY_2 = pkVBuffer.GetPosition3fY(i+2);
                fZ_2 = pkVBuffer.GetPosition3fZ(i+2);

                fX_3 = pkVBuffer.GetPosition3fX(i+3);
                fY_3 = pkVBuffer.GetPosition3fY(i+3);
                fZ_3 = pkVBuffer.GetPosition3fZ(i+3);

                pos_x = B_SPLINE(u, u_2, u_3, fX_0, fX_1, fX_2, fX_3);
                pos_y = B_SPLINE(u, u_2, u_3, fY_0, fY_1, fY_2, fY_3);
                pos_z = B_SPLINE(u, u_2, u_3, fZ_0, fZ_1, fZ_2, fZ_3);

                int iIndex = kTract.get(i);

                iIndex = kTract.get(i);

                int iX = iIndex % iDimX;
                iIndex -= iX;
                iIndex /= iDimX;

                int iY = iIndex % iDimY;
                iIndex -= iY;
                iIndex /= iDimY;

                int iZ = iIndex;

                float fX = (float)(iX)/(float)(iDimX);
                float fY = (float)(iY)/(float)(iDimY);
                float fZ = (float)(iZ)/(float)(iDimZ);

                resultUnit0 = new ColorRGB(fX, fY, fZ);

                iIndex = kTract.get(i);

                if (m_kImage.isColorImage()) {
                    fR = m_kImage.getFloat(iIndex * 4 + 1) / 255.0f;
                    fG = m_kImage.getFloat(iIndex * 4 + 2) / 255.0f;
                    fB = m_kImage.getFloat(iIndex * 4 + 3) / 255.0f;
                    resultUnit1 = new ColorRGB(fR, fG, fB);
                } else {
                    fR = m_kImage.getFloat(iIndex);
                    resultUnit1 = new ColorRGB(fR, fR, fR);
                }

                bsplineVBuffer.SetPosition3(index, pos_x, pos_y, pos_z);
                bsplineVBuffer.SetColor3(0, index, resultUnit0);
                bsplineVBuffer.SetColor3(1, index, resultUnit1);

                index++;

            }
        }

        return bsplineVBuffer;

    }

    /**
     * B-spline computation. 
     * @param u       u parameter 
     * @param u_2     u^2 parameter
     * @param u_3     u^3 parameter
     * @param cntrl0  1st control point coordinate
     * @param cntrl1  2nd control point coordinate
     * @param cntrl2  3rd control point coordinate
     * @param cntrl3  4th control point coordinate
     * @return  interpolated position
     */
    private float B_SPLINE(float u, float u_2, float u_3, float cntrl0, float cntrl1, float cntrl2, float cntrl3) {

        return (( 
                (-1*u_3 + 3*u_2 - 3*u + 1) * (cntrl0) + 
                ( 3*u_3 - 6*u_2 + 0*u + 4) * (cntrl1) + 
                (-3*u_3 + 3*u_2 + 3*u + 1) * (cntrl2) + 
                ( 1*u_3 + 0*u_2 + 0*u + 0) * (cntrl3)   
        ) / 6f);
    }


    /**
     * Add a polyline to the GPUVolumeRender.
     * 
     * @param kLine
     *            the Polyline to add.
     */
    protected void addPolyline(Polyline kLine) {
        m_kVolumeDisplay.addTract(kLine, m_iBundleCount, centerIndex);
    }

    /** Removes the fiber bundle from the GPUVolumeRender and JPanelSurface. */
    private void removePolyline() {
        int start = 0;
        int[] aiSelected = m_kTractList.getSelectedIndices();

        DefaultListModel kList = (DefaultListModel) m_kTractList.getModel();
        int iHeaderLength = (new String("FiberBundle")).length();

        for (int i = 0; i < aiSelected.length; i++) {
            if (m_kVolumeDisplay != null) {
                String kName = ((String) (kList.elementAt(aiSelected[i])));
                int iLength = kName.length();
                int iGroup = (new Integer(kName.substring(iHeaderLength,
                        iLength))).intValue();
                if ( (aiSelected[i] - 1) != -1 ) {
                    kName = ((String) (kList.elementAt((aiSelected[i] - 1))));
                    iLength = kName.length();
                    start = (new Integer(kName.substring(iHeaderLength,
                            iLength))).intValue();
                }
                for ( int j = start; j < iGroup; j++ ) {
                    m_kVolumeDisplay.removePolyline(j);	
                }
                m_kBundleList.remove(new Integer(iGroup));
            }
            kList.remove(aiSelected[i]);
        }

        if (kList.size() == 0) {
            /*
			if (m_kDTIImage != null) {
				m_kDTIImage.disposeLocal();
			}
			m_kDTIImage = null;
             */
        } else {
            m_kTractList.setSelectedIndex(kList.size());
        }
    }	

    /** Creates the user-interface for the Fiber Bundle Tract panel.
     * @return JPanel containing the user-interface for the Fiber Bundle Tract panel.
     */
    private JPanel createTractPanel()
    {
        // Scroll panel that hold the control panel layout in order to use JScrollPane
        JPanel mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());

        JScrollPane scroller = new JScrollPane(mainScrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        JPanel kTractPanel = new JPanel(new BorderLayout());

        JPanel buttonPanel = new JPanel();

        // buttons for add/remove of surfaces from list
        JButton addButton = new JButton("Add");

        addButton.addActionListener(this);
        addButton.setActionCommand("Add");
        addButton.setFont(MipavUtil.font12B);
        addButton.setPreferredSize(MipavUtil.defaultButtonSize);

        JButton removeButton = new JButton("Remove");

        removeButton.addActionListener(this);
        removeButton.setActionCommand("Remove");
        removeButton.setFont(MipavUtil.font12B);
        removeButton.setPreferredSize(MipavUtil.defaultButtonSize);

        buttonPanel.add(addButton);
        buttonPanel.add(removeButton);

        // list panel for surface filenames
        m_kTractList = new JList( new DefaultListModel() );
        m_kTractList.addListSelectionListener(this);
        m_kTractList.setPrototypeCellValue("aaaaaaaaaaaaaaaa.aaa    ");

        JScrollPane kScrollPane = new JScrollPane(m_kTractList);
        JPanel scrollPanel = new JPanel();

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel listPanel = new JPanel();
        listPanel.setLayout(new BorderLayout());
        listPanel.add(scrollPanel, BorderLayout.CENTER);
        listPanel.add(buttonPanel, BorderLayout.SOUTH);
        listPanel.setBorder(buildTitledBorder("Fiber Bundle list"));

        JPanel paintTexturePanel = new JPanel();
        paintTexturePanel.setLayout(new FlowLayout(FlowLayout.LEFT));

        m_kColorButton = new JButton("   ");
        m_kColorButton.setToolTipText("Change fiber bundle color");
        m_kColorButtonDefault = m_kColorButton.getBackground( );
        m_kColorButton.addActionListener(this);
        m_kColorButton.setActionCommand("ChangeColor");

        JLabel kColorLabel = new JLabel("Fiber Bundle color");
        kColorLabel.setFont(MipavUtil.font12B);
        kColorLabel.setForeground(Color.black);

        m_kUseVolumeColor = new JCheckBox("Use volume color" );
        m_kUseVolumeColor.addActionListener(this);
        m_kUseVolumeColor.setActionCommand("VolumeColor");
        m_kUseVolumeColor.setSelected(true);

        m_kUseEllipsoids = new JCheckBox("Use Ellipsoids" );
        m_kUseEllipsoids.addActionListener(this);
        m_kUseEllipsoids.setActionCommand("UseEllipsoids");
        m_kUseEllipsoids.setSelected(false);

        m_kAllEllipsoids = new JCheckBox("Display All Ellipsoids" );
        m_kAllEllipsoids.addActionListener(this);
        m_kAllEllipsoids.setActionCommand("AllEllipsoids");
        m_kAllEllipsoids.setSelected(false);

        m_kDisplaySlider = new JSlider(1, 500, 450);
        m_kDisplaySlider.setEnabled(true);
        m_kDisplaySlider.setMinorTickSpacing(10);
        m_kDisplaySlider.setPaintTicks(true);
        m_kDisplaySlider.addChangeListener(this);
        m_kDisplaySlider.setVisible(true);

        JLabel kSliderLabel = new JLabel("Display ellipsoids every X step: ");
        kSliderLabel.setFont(MipavUtil.font12B);
        kSliderLabel.setForeground(Color.black);


        JPanel colorPanel = new JPanel();
        colorPanel.setLayout(new BorderLayout());
        colorPanel.add(m_kColorButton, BorderLayout.WEST);
        colorPanel.add(kColorLabel, BorderLayout.CENTER);
        colorPanel.add(m_kUseVolumeColor, BorderLayout.EAST);
        colorPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        colorPanel.setAlignmentY(Component.TOP_ALIGNMENT);

        JPanel sliderPanel = new JPanel();
        sliderPanel.setLayout(new BorderLayout());
        sliderPanel.add(kSliderLabel, BorderLayout.WEST);
        sliderPanel.add(m_kDisplaySlider, BorderLayout.CENTER);
        sliderPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        sliderPanel.setAlignmentY(Component.TOP_ALIGNMENT);

        slicePickableCheckBox = new JCheckBox("Slice Pickable");
        slicePickableCheckBox.setSelected(false);
        slicePickableCheckBox.addActionListener(this);
        slicePickableCheckBox.setActionCommand("Pickable");
        slicePickableCheckBox.setEnabled(false);

        JPanel slicePanel = new JPanel();
        slicePanel.setLayout(new BorderLayout());
        slicePanel.add(slicePickableCheckBox, BorderLayout.WEST);
        slicePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        slicePanel.setAlignmentY(Component.TOP_ALIGNMENT);

        ButtonGroup group1 = new ButtonGroup();

        radioLines = new JRadioButton("Lines", true);
        radioLines.setFont(MipavUtil.font12B);
        radioLines.addItemListener(this);
        group1.add(radioLines);

        radioEllipzoids = new JRadioButton("Ellipzoid", false);
        radioEllipzoids.setFont(MipavUtil.font12B);
        radioEllipzoids.addItemListener(this);
        group1.add(radioEllipzoids);

        radioCylinders = new JRadioButton("Cylinders", false);
        radioCylinders.setFont(MipavUtil.font12B);
        radioCylinders.addItemListener(this);
        group1.add(radioCylinders);

        radioTubes = new JRadioButton("Tubes", false);
        radioTubes.setFont(MipavUtil.font12B);
        radioTubes.addItemListener(this);
        group1.add(radioTubes);

        radioArrows = new JRadioButton("Arrows", false);
        radioArrows.setFont(MipavUtil.font12B);
        radioArrows.addItemListener(this);
        radioArrows.setEnabled(false);
        group1.add(radioArrows);

        displayAllCheckBox = new JCheckBox("Display All");
        displayAllCheckBox.setSelected(false);
        displayAllCheckBox.addActionListener(this);
        displayAllCheckBox.setActionCommand("DisplayAll");
        displayAllCheckBox.setEnabled(true);

        JPanel glyphsPanel = new JPanel();
        glyphsPanel.setLayout(new BoxLayout(glyphsPanel, BoxLayout.X_AXIS));
        glyphsPanel.add(radioLines);
        glyphsPanel.add(radioEllipzoids);
        glyphsPanel.add(radioCylinders);
        glyphsPanel.add(radioTubes);
        glyphsPanel.add(radioArrows);
        glyphsPanel.add(displayAllCheckBox, BorderLayout.CENTER);
        glyphsPanel.setBorder(buildTitledBorder("Glyphs & Streamlines"));

        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.Y_AXIS));
        optionsPanel.add(colorPanel);
        optionsPanel.add(sliderPanel);
        optionsPanel.add(slicePanel);
        optionsPanel.setBorder(buildTitledBorder("Fiber bundle options"));

        JPanel rightPanel = new JPanel();
        rightPanel.setLayout(new BorderLayout());
        rightPanel.add(optionsPanel, BorderLayout.NORTH);

        Box contentBox = new Box(BoxLayout.Y_AXIS);

        contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        contentBox.add(listPanel);
        contentBox.add(rightPanel);
        contentBox.add(glyphsPanel);

        mainScrollPanel.add(contentBox, BorderLayout.NORTH);

        kTractPanel.add(scroller, BorderLayout.CENTER);

        return kTractPanel;
    }


    /**
     * Get the main control Panel.
     *
     * @return  mainPanel main control panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Cancel the color dialog, change nothing.
     */
    class CancelListener implements ActionListener {

        /**
         * Do nothing.
         * 
         * @param e
         *            action event
         */
        public void actionPerformed(ActionEvent e) {
        }
    }

    /**
     * Pick up the selected color and call method to change the fiber bundle
     * color.
     */
    class OkColorListener implements ActionListener {

        /**
         * Sets the button color to the chosen color and changes the color of
         * the fiber bundle.
         * 
         * @param e
         *            Event that triggered this method.
         */
        public void actionPerformed(ActionEvent e) {
            Color color = m_kColorChooser.getColor();

            m_kColorButton.setBackground(color);
            setColor(color);
        }
    }    

    /**
     * This is called when the user chooses a new color for the fiber bundle. It
     * changes the color of the fiber bundle.
     * 
     * @param color
     *            Color to change fiber bundle to.
     */
    private void setColor(Color color) {
        int[] aiSelected = m_kTractList.getSelectedIndices();
        DefaultListModel kList = (DefaultListModel) m_kTractList.getModel();
        int iHeaderLength = (new String("FiberBundle")).length();

        for (int i = 0; i < aiSelected.length; i++) {
            if (m_kVolumeDisplay != null) {
                String kName = ((String) (kList.elementAt(aiSelected[i])));
                int iLength = kName.length();
                int iGroup = (new Integer(kName.substring(iHeaderLength,
                        iLength))).intValue();
                if (color == null) {
                    m_kVolumeDisplay.setPolylineColor(iGroup, null);
                }

                else if (!m_kUseVolumeColor.isSelected()) {
                    m_kVolumeDisplay.setPolylineColor(iGroup, new ColorRGB(
                            color.getRed() / 255.0f, color.getGreen() / 255.0f,
                            color.getBlue() / 255.0f));
                }
            }
        }
    }


    /**
     * Reads a single fiber bundle tract from disk.
     * 
     * @param kFileReader
     *            FileInputStream.
     * @return Vector<Integer> fiber bundle tract -- list of voxel indices in
     *         order in which they appear in the tract.
     */
    private Vector<Integer> inputTract(FileInputStream kFileReader) {
        int iVQuantity = 0;
        int iBufferSize = 4;

        byte[] racBuffer = new byte[iBufferSize];
        try {
            kFileReader.read(racBuffer, 0, iBufferSize);
        } catch (IOException e1) {
        }
        ByteArrayInputStream acBufferIn = new ByteArrayInputStream(racBuffer);
        DataInputStream acDataIn = new DataInputStream(acBufferIn);
        try {
            iVQuantity = acDataIn.readInt();
        } catch (IOException e) {
            e.printStackTrace();
        }
        acBufferIn = null;
        acDataIn = null;
        racBuffer = null;

        iBufferSize = 4 * iVQuantity;
        racBuffer = new byte[iBufferSize];
        try {
            kFileReader.read(racBuffer, 0, iBufferSize);
        } catch (IOException e1) {
        }
        acBufferIn = new ByteArrayInputStream(racBuffer);
        acDataIn = new DataInputStream(acBufferIn);

        Vector<Integer> kTract = new Vector<Integer>();
        for (int i = 0; i < iVQuantity; i++) {
            try {
                kTract.add(acDataIn.readInt());
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        acBufferIn = null;
        acDataIn = null;

        return kTract;
    }	

    /**
     * Determines if the input tract is contained within the ModelImage
     * representing user-selected VOIs.
     * 
     * @param kVOIImage
     *            user-selected VOI image.
     * @param kTract
     *            list of voxels in the current fiber bundle tract.
     * @return true if the tract passes through the VOI or if the VOIImage is
     *         null, false otherwise.
     */
    private boolean contains(ModelImage kVOIImage, Vector<Integer> kTract) {
        if (kVOIImage == null) {
            return true;
        }
        for (int i = 0; i < kTract.size(); i++) {
            int iIndex = kTract.get(i);
            if (kVOIImage.getBoolean(iIndex)) {
                return true;
            }
        }
        return false;
    }


    /** Constructs the Fiber Bundle Tracts from the dtiImage and the
     * eigenImage parameters. The fiber bundles are output to a file
     * sepecified by the user.
     * @param dtiImage Diffusion Tensor Image.
     * @param eigenImage EigenVector Image.
     */
    public void diplayTract(int iX, int iY, int iZ)
    {
        m_kDTIImage = parentFrame.getDTIimage();
        int iDimX = m_kDTIImage.getExtents()[0];
        int iDimY = m_kDTIImage.getExtents()[1];
        int iDimZ = m_kDTIImage.getExtents()[2];
        int iLen = m_kDTIImage.getExtents()[0] *
        m_kDTIImage.getExtents()[1] * m_kDTIImage.getExtents()[2];

        float[] afVectorData = new float[3];

        m_abVisited  = new boolean[iLen];
        for ( int i = 0; i < iLen; i++ )
        {
            m_abVisited[i] = false;
        }

        Vector<Integer> kTract = new Vector<Integer>();
        Vector3f kPos = new Vector3f();
        Vector3f kV1 = new Vector3f();
        Vector3f kV2 = new Vector3f();

        int i = iZ * (iDimY*iDimX) + iY * iDimX + iX;
        centerIndex = i;

        boolean bAllZero = true;
        for ( int j = 0; j < 3; j++ )
        {
            afVectorData[j] = parentFrame.getEVimage().getFloat(i + j*iLen);
            if ( afVectorData[j] != 0 )
            {
                bAllZero = false;
            }
        }
        if ( !bAllZero )
        {        
            kPos.Set( iX, iY, iZ );
            kTract.add(i);

            kV1.Set( afVectorData[0], afVectorData[1], afVectorData[2] );
            kV2.Copy(kV1);
            kV2.Neg();

            kV1.Normalize();
            kV2.Normalize();

            traceTract( kTract, kPos, kV1, m_kDTIImage, true );
            m_abVisited[i] = true;
            traceTract( kTract, kPos, kV2, m_kDTIImage, false );
            int iVQuantity = kTract.size();
            addTract(kTract, iVQuantity, iDimX, iDimY, iDimZ);
        }
    }

    /** Traces a single fiber bundle tract starting at the input
     * position and following the input direction.
     * @param kTract fiber bundle tract, new positions are stored in this tract as the fiber is traced.
     * @param kStart starting positon of the tract.
     * @param kDir direction from the position.
     * @param dtiImage Diffusion Tensor image used to calculate next direction of tract.
     * @param bDir boolean when true the positions are added to the
     * end of the tract (positive direction). When false the positions
     * are added to the beginning of the tract (negative direction).
     */
    private void traceTract( Vector<Integer> kTract, Vector3f kStart, Vector3f kDir,
            ModelImage dtiImage, boolean bDir )
    {
        int iDimX = dtiImage.getExtents()[0];
        int iDimY = dtiImage.getExtents()[1];
        int iDimZ = dtiImage.getExtents()[2];
        int iLen = dtiImage.getExtents()[0] * dtiImage.getExtents()[1] * dtiImage.getExtents()[2];

        float[] afTensorData = new float[6];

        boolean bDone = false;
        Matrix3f kMatrix = new Matrix3f();
        Vector3f kOut = new Vector3f();
        Vector3f kNext = new Vector3f();
        int iX;
        int iY;
        int iZ;
        int i;
        boolean bAllZero = true;

        while ( !bDone )
        {
        	kNext.Add( kStart, kDir );
            iX = Math.round(kNext.X);
            iY = Math.round(kNext.Y);
            iZ = Math.round(kNext.Z);
            i = iZ * (iDimY*iDimX) + iY * iDimX + iX;

            if ( (iZ < 0) || (iZ >= iDimZ) ||
                    (iY < 0) || (iY >= iDimY) ||
                    (iX < 0) || (iX >= iDimX)  )
            {
                bDone = true;
                break;
            }

            bAllZero = true;
            for ( int j = 0; j < 6; j++ )
            {
                afTensorData[j] = dtiImage.getFloat(i + j*iLen);
                if ( afTensorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            if ( !bAllZero )
            {
                kMatrix.Set( afTensorData[0], afTensorData[3], afTensorData[4],
                        afTensorData[3], afTensorData[1], afTensorData[5], 
                        afTensorData[4], afTensorData[5], afTensorData[2] );

                kMatrix.Mult(kDir, kOut);
                kOut.Normalize();

                if ( m_abVisited[i] )
                {
                    bDone = true;
                    break;
                }
                m_abVisited[i] = true;

                if ( bDir )
                {
                    kTract.add( i );
                }
                else
                {
                    kTract.add( 0, i );
                }

                kStart = kNext;
                kDir = kOut;
            }
            else
            {
                bDone = true;
            }
        }
        kNext = null;
    }    

    /**
     * Sets the flags for the checkboxes.
     *
     * @param  event  event that triggered this function
     */
    public synchronized void itemStateChanged(ItemEvent event) {
        if (radioLines.isSelected()) {
            displayMode = Polylines;
        } else if ( radioEllipzoids.isSelected()) {
            displayMode = Ellipzoids;
        } else if ( radioTubes.isSelected()) {
            displayMode = Tubes;
        } else if ( radioCylinders.isSelected() ) {
            displayMode = Cylinders;
        } else if ( radioArrows.isSelected()) {
            displayMode = Arrows;
        }
        invokeDisplayFunction();
    }

    public void invokeDisplayFunction() {
        if ( displayMode == Ellipzoids && displayAll == false) {
            parentFrame.getLightControl().refreshLighting();
            m_kVolumeDisplay.setDisplayEllipsoids(true);
            m_kVolumeDisplay.setDisplayAllEllipsoids(false);
            m_kVolumeDisplay.setDisplayCylinders(false);
            m_kVolumeDisplay.setDisplayAllCylinders(false);
            m_kVolumeDisplay.setDisplayTubes(false);
        } else if (displayMode == Ellipzoids && displayAll == true) {
            parentFrame.getLightControl().refreshLighting();
            Color color = m_kColorButton.getBackground();
            m_kVolumeDisplay.setDisplayAllEllipsoids(true);
            m_kVolumeDisplay.setDisplayEllipsoids(false);
            m_kVolumeDisplay.setDisplayCylinders(false);
            m_kVolumeDisplay.setDisplayAllCylinders(false);
            m_kVolumeDisplay.setDisplayTubes(false);
        } else if (displayMode == Cylinders && displayAll == false) {
            parentFrame.getLightControl().refreshLighting();
            m_kVolumeDisplay.setDisplayCylinders(true);
            m_kVolumeDisplay.setDisplayAllEllipsoids(false);
            m_kVolumeDisplay.setDisplayEllipsoids(false);
            m_kVolumeDisplay.setDisplayAllCylinders(false);
            m_kVolumeDisplay.setDisplayTubes(false);
        } else if (displayMode == Cylinders && displayAll == true) {
            parentFrame.getLightControl().refreshLighting();
            Color color = m_kColorButton.getBackground();
            m_kVolumeDisplay.setDisplayAllCylinders(true);
            m_kVolumeDisplay.setDisplayCylinders(false);
            m_kVolumeDisplay.setDisplayAllEllipsoids(false);
            m_kVolumeDisplay.setDisplayEllipsoids(false);
            m_kVolumeDisplay.setDisplayTubes(false);
        }  else if (displayMode == Tubes /* && displayAll == false */) {
            parentFrame.getLightControl().refreshLighting();
            Color color = m_kColorButton.getBackground();
            m_kVolumeDisplay.setDisplayTubes(true);
            m_kVolumeDisplay.setDisplayAllCylinders(false);
            m_kVolumeDisplay.setDisplayCylinders(false);
            m_kVolumeDisplay.setDisplayAllEllipsoids(false);
            m_kVolumeDisplay.setDisplayEllipsoids(false);
        }  else if (displayMode == Polylines ) {
            m_kVolumeDisplay.setDisplayEllipsoids(false);
            m_kVolumeDisplay.setDisplayAllEllipsoids(false);
            m_kVolumeDisplay.setDisplayCylinders(false);
            m_kVolumeDisplay.setDisplayAllCylinders(false);
            m_kVolumeDisplay.setDisplayTubes(false);
        }
    }


}