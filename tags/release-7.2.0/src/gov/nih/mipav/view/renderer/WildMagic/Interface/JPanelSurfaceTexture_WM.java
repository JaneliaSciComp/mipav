package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.JFrameHistogram;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.JPanelHistoLUT;
import gov.nih.mipav.view.renderer.JPanelHistoRGB;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;


/**
 * JPanelSurfaceTexture. Enables texture-mapping of the ModelImage data onto a surface triangle mesh. 
 *
 * <p>The user can change the ModelLUT independently of the ModelLUT associated with the ModelImage, or if the user
 * selects the option of using the ModelLUT associated with the ModelImage, then the texture updates as the user updates
 * that LUT. This class implements the ViewImageUpdateInterface to capture LUT changes.</p>
 *
 */
public class JPanelSurfaceTexture_WM extends JInterfaceBase implements ViewImageUpdateInterface {

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7562070328632922435L;

    /** Display the independent LUT for Black/White images. */
    private JFrameHistogram mHistogram;
    //private JPanelHistoLUT mHistoLUT;

    /** Display the independent RGB for Color Images. */
    //private JPanelHistoRGB mHistoRGB;

    /** ModelImage used to generate the 3D texture:. */
    private ModelImage mImageA;

    /** Reference to ModelImage A for linking the texture to the imageA LUT. */
    private ModelImage mImageALink;

    /** RadioButton for turing on the surface image texture:. */
    private JCheckBox mImageAsTextureCheck;

    /** Stores the currently-loaded ModelImage directory name:. */
    private String mImageDirName;

    /** Stores the currently-loaded ModelImage file name:. */
    private String mImageFileName;

    /** Display the currently-loaded ModelImage file name:. */
    private JLabel mImageFileNameLabel;

    /** Load a new ModelImage:. */
    private JButton mLoadImageButton;

    /** Use the ModelImage LUT. */
    private JRadioButton mModelImageRadioButton;

    /** Use a separate LUT. */
    private JRadioButton mNewImageRadioButton;

    /** Grouping the radio buttons:. */
    private ButtonGroup mImageButtonGroup = new ButtonGroup();
    
    /** Independent ModelImage for independent LUT. */
    private ModelImage mLUTImageA;

    /** The LUT associated with the ModelImage imageA:. */
    private ModelStorageBase mLUTModel = null;

    /** The LUT associated with the independent texture LUT:. */
    private ModelStorageBase mLUTSeparate = null;

    /** Use the ModelImage LUT. */
    private JRadioButton mModelLUTRadioButton;

    /** Use a separate LUT. */
    private JRadioButton mNewLUTRadioButton;

    /** Grouping the radio buttons:. */
    private ButtonGroup mLUTButtonGroup = new ButtonGroup();
    
    /** Surface panel */
    private JPanelSurface_WM m_kSurfacePanel = null;

    /**
     * Constructor.
     * @param kVolumeViewer parent frame.
     */
    public JPanelSurfaceTexture_WM( VolumeTriPlanarInterface kVolumeViewer ) {
        super(kVolumeViewer);
        mImageA = kVolumeViewer.getImageA();
        mImageALink = mImageA;
        mImageALink.addImageDisplayListener(this);
        init();
    }

    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("LoadNewImage")) {
            JFileChooser chooser = new JFileChooser();
            chooser.setMultiSelectionEnabled(false);
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            if (JFileChooser.APPROVE_OPTION != chooser.showOpenDialog(null)) {
                return;
            }

            mImageFileName = chooser.getSelectedFile().getName();
            mImageDirName = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            mImageFileNameLabel.setText(mImageFileName);
            chooser.setVisible(false);

            loadingImage();

        } else if (command.equals("OriginalModelImage")) {
            if ( m_kSurfacePanel != null )
            {
                m_kSurfacePanel.ImageAsTexture(mImageAsTextureCheck.isSelected(),
                        mNewImageRadioButton.isSelected(),
                        mNewLUTRadioButton.isSelected() );
            }
        } else if (command.equals("LoadedModelImage")) {
            if ( m_kSurfacePanel != null )
            {
                m_kSurfacePanel.ImageAsTexture(mImageAsTextureCheck.isSelected(),
                        mNewImageRadioButton.isSelected(),
                        mNewLUTRadioButton.isSelected() );
            }
        } else if (command.equals("ImageAsTexture")) {
            if ( m_kSurfacePanel != null )
            {
                m_kSurfacePanel.ImageAsTexture(mImageAsTextureCheck.isSelected(),
                        mNewImageRadioButton.isSelected(),
                        mNewLUTRadioButton.isSelected() );
            }
        } else if (command.equals("LinkLUTs")) {

            mainPanel.remove(mHistogram.getContainingPanel());
//            if (!mImageA.isColorImage()) {
//                mainPanel.remove(mHistoLUT.getMainPanel());
//            } else {
//                mainPanel.remove(mHistoRGB.getMainPanel());
//            }

            mainPanel.updateUI();
            //updateImages(null, mLUTModel, false, 0);
            if ( m_kSurfacePanel != null )
            {
                m_kSurfacePanel.ImageAsTexture(mImageAsTextureCheck.isSelected(),
                        mNewImageRadioButton.isSelected(),
                        mNewLUTRadioButton.isSelected() );
            }
        } else if (command.equals("SeparateLUTs")) {

            mainPanel.add(mHistogram.getContainingPanel(), BorderLayout.SOUTH);
//            if (!mImageA.isColorImage()) {
//                mainPanel.add(mHistoLUT.getMainPanel(), BorderLayout.SOUTH);
//            } else {
//                mainPanel.add(mHistoRGB.getMainPanel(), BorderLayout.SOUTH);
//            }

            mainPanel.updateUI();
            //updateImages(mLUTSeparate, null, false, 0);
            if ( m_kSurfacePanel != null )
            {
                m_kSurfacePanel.ImageAsTexture(mImageAsTextureCheck.isSelected(),
                        mNewImageRadioButton.isSelected(),
                        mNewLUTRadioButton.isSelected() );
            }
        }
    }

    /**
     * Removes this object from the ModelImage imageDisplayListener list.
     */
    public void disposeLocal() {
        if (mLUTImageA != null) {
            mLUTImageA.disposeLocal();
            mLUTImageA = null;
        }
        mImageALink.removeImageDisplayListener(this);
        if ( mHistogram != null )
        {
        	mHistogram.disposeLocal();
        	mHistogram = null;
        }
    }

    /**
     * Returns The SurfaceRender ModelImage imageA for linking to the LUT.
     * @return  mImageALink, for identifying the ModelLUT associated with mImageA.
     */
    public ModelImage getImageLink() {
        return mImageALink;
    }

    /**
     * Returns the ModelImage associated with the independent LUT.
     * @return  the ModelImage associated with the independent LUT
     */
    public ModelImage getImageSeparate() {
        return mLUTImageA;
    }

    /**
     * Return the current ModelLUT.
     * @return  the currently used ModelLUT.
     */
    public ModelStorageBase getLUT() {

        if (mModelLUTRadioButton.isSelected()) {
            return mLUTModel;
        }

        return mLUTSeparate;
    }

    public ModelStorageBase getSeparateLUT()
    {
        return mLUTSeparate;
    }

    public void setSeparateLUT(ModelStorageBase kLUT)
    {
        if ( kLUT != null )
        {
            mLUTSeparate = kLUT;
            initLUT();
            m_kSurfacePanel.SetLUTNew( mLUTSeparate );
            m_kSurfacePanel.ImageAsTexture(mImageAsTextureCheck.isSelected(),
                    mNewImageRadioButton.isSelected(),
                    mNewLUTRadioButton.isSelected() );
        }
    }
    
    /**
     * Returns The ModelImage that is the data source for the Texture3D.
     * @return  mImageA, the ModelImage used to generate the Texture3D
     */
    public ModelImage getTextureImage() {
        return mImageA;
    }
    
    public String getImageFileName()
    {
        if ( mImageFileName == null )
        {
            return null;
        }
        return new String(mImageFileName);
    }
    
    public String getImageDir()
    {
        if ( mImageDirName == null )
        {
            return null;
        }
        return new String(mImageDirName);
    }

    public void setTextureImage(String kDir, String kFileName ) {
        if ( kDir != null && kFileName != null )
        {
            mImageFileName = kFileName;
            mImageDirName = kDir;
            mImageFileNameLabel.setText(mImageFileName);
            loadingImage();
        }
    }

    
    public boolean getTextureImageOn() {
        return mNewImageRadioButton.isSelected();
    }
    
    public void setTextureImageOn(boolean bOn) {
        mNewImageRadioButton.setSelected(bOn);
        m_kSurfacePanel.ImageAsTexture(mImageAsTextureCheck.isSelected(),
                mNewImageRadioButton.isSelected(),
                mNewLUTRadioButton.isSelected() );
    }
    
    public boolean getTextureLUTOn() {
        return mNewLUTRadioButton.isSelected();
    }
    
    public void setTextureLUTOn(boolean bOn) {
        mNewLUTRadioButton.setSelected(bOn);
        m_kSurfacePanel.ImageAsTexture(mImageAsTextureCheck.isSelected(),
                mNewImageRadioButton.isSelected(),
                mNewLUTRadioButton.isSelected() );
    }

    public boolean getTextureOn()
    {
        return mImageAsTextureCheck.isSelected();
    }

    public void setTextureOn(boolean bOn)
    {
        mImageAsTextureCheck.setSelected(bOn);
        m_kSurfacePanel.ImageAsTexture(mImageAsTextureCheck.isSelected(),
                mNewImageRadioButton.isSelected(),
                mNewLUTRadioButton.isSelected() );
    }

    public boolean getEnabled()
    {
        return mImageAsTextureCheck.isEnabled();
    }
    
    /**
     * Enables or disables the interface. Called when a surface is added/removed from the JPanelSurface class.
     * @param  flag  when true enable the interface, when false disable the interface.
     */
    public void setEnabled(boolean flag) {
        mImageAsTextureCheck.setEnabled(flag);
        mModelLUTRadioButton.setEnabled(flag);
        mNewLUTRadioButton.setEnabled(flag);
    }


    /**
     * Update the ModelRGB associated with the separate texture, and regenerate the ImageComponente3D volume texture.
     * @param  RGBTa  the new ModelRGB for the separate texture.
     */
    public void setRGBTA(ModelRGB RGBTa) {

        if (RGBTa != null) {
        	mLUTSeparate = RGBTa;
            if ( m_kSurfacePanel != null )
            {
                m_kSurfacePanel.SetLUTNew( mLUTSeparate );
            }
        }
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#setSlice(int)
     */
    public void setSlice(int slice) { }

    /**
     * Sets the surface panel.
     * @param kSurfacePanel surface panel.
     */
    public void setSurfacePanel( JPanelSurface_WM kSurfacePanel )
    {
        m_kSurfacePanel = kSurfacePanel;
        m_kSurfacePanel.SetLUTNew( mLUTSeparate );
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#setTimeSlice(int)
     */
    public void setTimeSlice(int tSlice) { }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImageExtents()
     */
    public boolean updateImageExtents() {
        return false;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages()
     */
    public boolean updateImages() {
        return false;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages(boolean)
     */
    public boolean updateImages(boolean flag) {
        return false;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages(gov.nih.mipav.model.structures.ModelLUT, gov.nih.mipav.model.structures.ModelLUT, boolean, int)
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
        if ( m_kSurfacePanel != null )
        {
            m_kSurfacePanel.ImageAsTexture(mImageAsTextureCheck.isSelected(),
                    mNewImageRadioButton.isSelected(),
                    mNewLUTRadioButton.isSelected() );
            m_kSurfacePanel.SetLUTNew( mLUTSeparate );
        }
        return true;
    }

    /**
     * Initializes the interface, and generates the first default texture.
     */
    private void init() {
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;

        JPanel imagePanel = new JPanel();
        imagePanel.setBorder(buildTitledBorder("Image source options"));
        imagePanel.setLayout(new GridBagLayout());
        imagePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        gbc.gridx = 0;
        gbc.gridy = 0;

        /* Button: Load Surface */
        mLoadImageButton = new JButton();
        mLoadImageButton.setText("Select Image ...");
        mLoadImageButton.setActionCommand("LoadNewImage");
        mLoadImageButton.addActionListener(this);
        imagePanel.add(mLoadImageButton, gbc);      
        gbc.gridx++;

        /* Label: name of loaded surface file */
        mImageFileNameLabel = new JLabel();
        mImageFileNameLabel.setPreferredSize(new Dimension(130, 21));
        mImageFileNameLabel.setBorder(BorderFactory.createLoweredBevelBorder());
        mImageFileName = mImageA.getImageFileName();
        mImageFileNameLabel.setText(mImageFileName);
        imagePanel.add(mImageFileNameLabel, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;
        mModelImageRadioButton = new JRadioButton("Use Original ModelImage");
        mModelImageRadioButton.addActionListener(this);
        mModelImageRadioButton.setActionCommand("OriginalModelImage");
        mModelImageRadioButton.setSelected(true);
        mModelImageRadioButton.setEnabled(false);
        mImageButtonGroup.add(mModelImageRadioButton);
        imagePanel.add(mModelImageRadioButton, gbc);

        gbc.gridx++;
        mNewImageRadioButton = new JRadioButton("Use Loaded ModelImage");
        mNewImageRadioButton.addActionListener(this);
        mNewImageRadioButton.setActionCommand("LoadedModelImage");
        mNewImageRadioButton.setSelected(false);
        mNewImageRadioButton.setEnabled(false);
        mImageButtonGroup.add(mNewImageRadioButton);
        imagePanel.add(mNewImageRadioButton, gbc);


        JPanel texturePanel = new JPanel();
        texturePanel.setBorder(buildTitledBorder("Image display options"));
        texturePanel.setLayout(new GridBagLayout());
        texturePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        gbc.gridx = 0;
        gbc.gridy = 0;

        mImageAsTextureCheck = new JCheckBox("3D Texture");
        mImageAsTextureCheck.addActionListener(this);
        mImageAsTextureCheck.setActionCommand("ImageAsTexture");
        mImageAsTextureCheck.setSelected(false);
        mImageAsTextureCheck.setEnabled(false);
        texturePanel.add(mImageAsTextureCheck, gbc);

        JPanel lutPanel = new JPanel();
        lutPanel.setBorder(buildTitledBorder("LUT options"));
        lutPanel.setLayout(new GridBagLayout());
        lutPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        gbc.gridx = 0;
        gbc.gridy = 0;

        mModelLUTRadioButton = new JRadioButton("Use ModelImage LUT");
        mModelLUTRadioButton.addActionListener(this);
        mModelLUTRadioButton.setActionCommand("LinkLUTs");
        mModelLUTRadioButton.setSelected(false);
        mModelLUTRadioButton.setEnabled(false);
        mLUTButtonGroup.add(mModelLUTRadioButton);
        lutPanel.add(mModelLUTRadioButton, gbc);

        gbc.gridx++;
        mNewLUTRadioButton = new JRadioButton("Use Separate LUT");
        mNewLUTRadioButton.addActionListener(this);
        mNewLUTRadioButton.setActionCommand("SeparateLUTs");
        mNewLUTRadioButton.setSelected(true);
        mNewLUTRadioButton.setEnabled(false);
        mLUTButtonGroup.add(mNewLUTRadioButton);
        lutPanel.add(mNewLUTRadioButton, gbc);


        JPanel controlsPanel = new JPanel(new BorderLayout());
        controlsPanel.add(imagePanel, BorderLayout.NORTH);
        controlsPanel.add(texturePanel, BorderLayout.CENTER);
        controlsPanel.add(lutPanel, BorderLayout.SOUTH);

        mainPanel = new JPanel(new BorderLayout());

        // mainPanel.setAlignmentY(Component.TOP_ALIGNMENT);
        mainPanel.add(controlsPanel, BorderLayout.NORTH);
        initLUT();
    }

    /**
     * Initializes or re-initializes the Histogram LUT interface based on the currently-loaded ModelImage.
     */
    private void initLUT() {

        /* Create LUT interface: */
        mImageA.calcMinMax();

        if (!mImageA.isColorImage()) {
            float fMin = (float) mImageA.getMin();
            float fMax = (float) mImageA.getMax();

            int[] iExtents = { 256, 256 };
            mLUTImageA = new ModelImage(ModelStorageBase.FLOAT, iExtents, "temp");
            mLUTImageA.addImageDisplayListener(this);

            for (int i = 0; i < 256; i++) {

                for (int j = 0; j < 256; j++) {
                    mLUTImageA.set(i, j, fMin + ( i / 255.0f * (fMax - fMin)));
                }
            }

            mLUTImageA.calcMinMax();

            /* Create LUT */
            int[] dimExtentsLUT = { 4, 256 };
            if ( mLUTSeparate == null )
            {
                mLUTSeparate = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
                ((ModelLUT)mLUTSeparate).resetTransferLine(fMin, fMin, fMax, fMax);
            }

            /* Create LUT panel: */
            //mHistoLUT = new JPanelHistoLUT(mLUTImageA, null, mLUTSeparate, null, true);
            mHistogram = new JFrameHistogram(m_kVolumeViewer, mLUTImageA, null, mLUTSeparate, null);
            mHistogram.histogramLUT(true, false);
        } else {
            float fMinR = (float) mImageA.getMinR();
            float fMaxR = (float) mImageA.getMaxR();

            float fMinG = (float) mImageA.getMinG();
            float fMaxG = (float) mImageA.getMaxG();

            float fMinB = (float) mImageA.getMinB();
            float fMaxB = (float) mImageA.getMaxB();

            int[] iExtents = { 256, 256 };
            mLUTImageA = new ModelImage(ModelStorageBase.ARGB_FLOAT, iExtents, "SurfaceTextureLUT");
            mLUTImageA.addImageDisplayListener(this);

            for (int j = 0; j < 256; j++) {

                for (int i = 0; i < 256; i++) {
                    mLUTImageA.setC(i, j, 0, 255.0f);
                    mLUTImageA.setC(i, j, 1, fMinR + (j / 255.0f * (fMaxR - fMinR)));
                    mLUTImageA.setC(i, j, 2, fMinG + (j / 255.0f * (fMaxG - fMinG)));
                    mLUTImageA.setC(i, j, 3, fMinB + (j / 255.0f * (fMaxB - fMinB)));
                }
            }

            mLUTImageA.calcMinMax();

            /* Create LUT */
            int[] dimExtentsLUT = { 4, 256 };
            if ( mLUTSeparate == null )
            {
            	mLUTSeparate = new ModelRGB(dimExtentsLUT);
            }            

            /* Create LUT panel: */
            mHistogram = new JFrameHistogram(m_kVolumeViewer, mLUTImageA, null, mLUTSeparate, null);
            mHistogram.histogramLUT(true, false);
        }

        mainPanel.add(mHistogram.getContainingPanel(), BorderLayout.SOUTH);

        mainPanel.updateUI();
        if ( m_kSurfacePanel != null )
        {
            m_kSurfacePanel.SetLUTNew( mLUTSeparate );
        }
    }

    /**
     * Load a new ModelImage to use for the 3D Texture display:
     */
    private void loadingImage() {

        /* If the ModelImage m_kImage is not defined (null) or the name
         * doesn't equal the name set in m_kImageFile, then load the new
         * ModelImage: */
        if ((mImageA == null) || ((mImageA != null) && (!mImageFileName.equals(mImageA.getImageName())))) {
            FileIO fileIO = new FileIO();
            fileIO.setQuiet(true);

            mImageA = fileIO.readImage(mImageFileName, mImageDirName, false, null);
            mImageFileName = mImageA.getImageFileName();
            mImageFileNameLabel.setText(mImageFileName);
            initLUT();
            mainPanel.updateUI();
            
            mModelImageRadioButton.setEnabled(true);
            mNewImageRadioButton.setEnabled(true);
            if ( m_kSurfacePanel != null )
            {
                m_kSurfacePanel.SetImageNew( mImageA );
            }
        }
    } 
}
