package gov.nih.mipav.view.renderer.surfaceview;

import java.io.*;
import java.awt.*;
import java.awt.image.*;
import java.awt.event.ActionEvent;
import java.util.*;
import javax.swing.JPanel;
import javax.media.j3d.*;
import javax.swing.*;
import javax.vecmath.*;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;

/**
 * JPanelSurfaceTexture. Enables texture-mapping of the ModelImage data onto a
 * surface triangle mesh. The Texture coordinates of the mesh are calculated
 * in the SurfaceMask class. This class creates the ImageComponent3D object
 * that is passed to the Texture3D object when the surface display attributes
 * (TextureUnitState) are created. The ImageComponent3D object contained
 * within this class updates when the user changes the ModelLUT associated
 * with the texture.
 *
 * The user can change the ModelLUT independently of the ModelLUT associated
 * with the ModelImage, or if the user selects the option of using the
 * ModelLUT associated with the ModelImage, then the texture updates as the
 * user updates that LUT. This class implements the ViewImageUpdateInterface
 * to capture LUT changes.
 *
 * @see JPanelSurface.java
 * @see SurfaceMask.java
 * @see ModelImage.java
 */
public class JPanelSurfaceTexture extends JPanelRendererBase
    implements ViewImageUpdateInterface
{
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7562070328632922435L;
    
    // Types of image-as-texture displays: 
    /** No display: */
    public static final int NONE = 0;
    /** Displays the ModelImage with 3D Texture-mapping : */
    public static final int TEXTURE = 1;
    /** Displays the ModelImage with per-vertex colors: */
    public static final int VERTEX_COLOR = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Currently selected ModelImage display type: */
    private int mTextureStatus = NONE;

    // Interface buttons:
    /** Load a new ModelImage: */
    private JButton mLoadImageButton;
    /** Display the currently-loaded ModelImage file name: */
    private JLabel mImageFileNameLabel; 
    /** Stores the currently-loaded ModelImage file name: */
    private String mImageFileName;
    /** Stores the currently-loaded ModelImage directory name: */
    private String mImageDirName;

    /** RadioButton for turing on the surface image texture: */
    private JRadioButton mImageAsTextureRadioButton;

    /** RadioButton for turing on the surface image per-vertex color: */
    private JRadioButton mImageAsVertexColorRadioButton;

    /** RadioButton for turing off the surface image: */
    private JRadioButton mImageNoneRadioButton;

    /** Grouping the radio buttons: */
    private ButtonGroup mTextureButtonGroup = new ButtonGroup();

    /** Grouping the radio buttons: */
    private ButtonGroup mLUTButtonGroup = new ButtonGroup();

    /** Use the ModelImage LUT */
    private JRadioButton mModelLUTRadioButton;

    /** Use a separate LUT */
    private JRadioButton mNewLUTRadioButton;

    /** ModelImage used to generate the 3D texture: */
    private ModelImage mImageA;

    /** Reference to ModelImage A for linking the texture to the imageA LUT */
    private ModelImage mImageALink;

    /** Independent ModelImage for independent LUT */
    private ModelImage mLUTImageA;

    /** Display the independent LUT for Black/White images*/
    private JPanelHistoLUT mHistoLUT;

    /** Display the independent RGB for Color Images */
    private JPanelHistoRGB mHistoRGB;

    /** The LUT associated with the ModelImage imageA: */
    private ModelLUT mLUTModel = null;

    /** The LUT associated with the independent texture LUT: */
    private ModelLUT mLUTSeparate = null;

    /** The RGB LUT associated with the ModelImage imageA: */
    private ModelRGB mRGBModel = null;

    /** The RGB LUT associated with the independent texture LUT: */
    private ModelRGB mRGBSeparate = null;

    /** The ModelImage texture with LUT changes, used with the Texture3D. */
    private ImageComponent3D mSurfaceTextureImage = null;

    /** Renders the ModelImage data with LUT changes */
    private PatientSlice mPatientSlice = null;

    /** Constructor:
     * @param kView, the SurfaceRender this panel is displayed with
     * @param imageA, the ModelImage used to generate the volume texture.
     */
    public JPanelSurfaceTexture( SurfaceRender kView, ModelImage imageA )
    {
        super(kView);
        mImageA = imageA;
        mImageALink = imageA;
        mImageALink.addImageDisplayListener( this );
        mPatientSlice = new PatientSlice( mImageA, null, null, null, FileInfoBase.UNKNOWN_ORIENT );
        init();
    }

    /**
     * Removes this object from the ModelImage imageDisplayListener list.
     */
    public void dispose()
    {
        mImageALink.removeImageDisplayListener( this );
    }


    /** 
     * Returns the volume texture data in ImageComponent3D form, with LUT
     * changes. Used to set the Texture3D in the JPanelSurface class when the
     * ModelTriangleMesh surface/display attributes are created.
     * @return mSurfaceTextureImage, the volume texture data with LUT changes.
     */
    public ImageComponent3D getSurfaceTextureImage()
    {
        return mSurfaceTextureImage;
    }

    /** Initializes the interface, and generates the first default texture.
     */
    private void init()
    {
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;

        JPanel imagePanel = new JPanel();
        imagePanel.setBorder(buildTitledBorder("Image source options"));
        imagePanel.setLayout(new GridBagLayout() );
        imagePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        gbc.gridx = 0;
        gbc.gridy = 0;

        /* Button: Load Surface */
        mLoadImageButton = new JButton();
        mLoadImageButton.setText("Select Image ...");
        mLoadImageButton.setActionCommand("LoadNewImage");
        mLoadImageButton.addActionListener(this);
        imagePanel.add( mLoadImageButton, gbc );

        gbc.gridx++;
        /* Label: name of loaded surface file */
        mImageFileNameLabel = new JLabel();
        mImageFileNameLabel.setPreferredSize(new Dimension(130, 21));
        mImageFileNameLabel.setBorder(BorderFactory.createLoweredBevelBorder());
        mImageFileName = mImageA.getImageName();
        mImageFileNameLabel.setText( mImageFileName );
        imagePanel.add( mImageFileNameLabel, gbc );


        JPanel texturePanel = new JPanel();
        texturePanel.setBorder(buildTitledBorder("Image display options"));
        texturePanel.setLayout(new GridBagLayout() );
        texturePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        gbc.gridx = 0;
        gbc.gridy = 0;

        mImageAsTextureRadioButton = new JRadioButton( "3D Texture" );
        mImageAsTextureRadioButton.addActionListener(this);
        mImageAsTextureRadioButton.setActionCommand("ImageAsTexture");
        mImageAsTextureRadioButton.setSelected(false);
        mImageAsTextureRadioButton.setEnabled(false);
        mTextureButtonGroup.add( mImageAsTextureRadioButton );
        texturePanel.add( mImageAsTextureRadioButton, gbc);

        gbc.gridx++;
        mImageAsVertexColorRadioButton = new JRadioButton( "Per-Vertex Color" );
        mImageAsVertexColorRadioButton.addActionListener(this);
        mImageAsVertexColorRadioButton.setActionCommand("ImageAsVertexColor");
        mImageAsVertexColorRadioButton.setSelected(false);
        mImageAsVertexColorRadioButton.setEnabled(false);
        mTextureButtonGroup.add( mImageAsVertexColorRadioButton );
        texturePanel.add( mImageAsVertexColorRadioButton, gbc);

        gbc.gridx++;
        mImageNoneRadioButton = new JRadioButton( "None" );
        mImageNoneRadioButton.addActionListener(this);
        mImageNoneRadioButton.setActionCommand("ImageNone");
        mImageNoneRadioButton.setSelected(true);
        mImageNoneRadioButton.setEnabled(false);
        mTextureButtonGroup.add( mImageNoneRadioButton );
        texturePanel.add(mImageNoneRadioButton, gbc);


        JPanel lutPanel = new JPanel();
        lutPanel.setBorder(buildTitledBorder("LUT options"));
        lutPanel.setLayout(new GridBagLayout() );
        lutPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        mModelLUTRadioButton = new JRadioButton("Use ModelImage LUT");
        mModelLUTRadioButton.addActionListener(this);
        mModelLUTRadioButton.setActionCommand("LinkLUTs");
        mModelLUTRadioButton.setSelected(false);
        mModelLUTRadioButton.setEnabled(false);
        mLUTButtonGroup.add( mModelLUTRadioButton );
        lutPanel.add(mModelLUTRadioButton, gbc);

        gbc.gridx++;
        mNewLUTRadioButton = new JRadioButton("Use Separate LUT");
        mNewLUTRadioButton.addActionListener(this);
        mNewLUTRadioButton.setActionCommand("SeparateLUTs");
        mNewLUTRadioButton.setSelected(true);
        mNewLUTRadioButton.setEnabled(false);
        mLUTButtonGroup.add( mNewLUTRadioButton );
        lutPanel.add(mNewLUTRadioButton, gbc);


        JPanel controlsPanel = new JPanel( new BorderLayout() );
        controlsPanel.add( imagePanel, BorderLayout.NORTH );
        controlsPanel.add( texturePanel, BorderLayout.CENTER );
        controlsPanel.add( lutPanel, BorderLayout.SOUTH );

        mainPanel = new JPanel( new BorderLayout() );
        //mainPanel.setAlignmentY(Component.TOP_ALIGNMENT);
        mainPanel.add( controlsPanel, BorderLayout.NORTH );
        initLUT();

        mSurfaceTextureImage = generateVolumeTexture();
    }

    /** 
     * Initializes or re-initializes the Histogram LUT interface based on the
     * currently-loaded ModelImage.
     */
    private void initLUT()
    {
        /* Create LUT interface: */
        mImageA.calcMinMax();
        
        if ( !mImageA.isColorImage() )
        {
            float fMin = (float)mImageA.getMin();
            float fMax = (float)mImageA.getMax();
            
            int[] iExtents = { 256, 256 };
            mLUTImageA = new ModelImage(ModelStorageBase.FLOAT, iExtents, "temp", ViewUserInterface.getReference());
            mLUTImageA.addImageDisplayListener(this);
            
            for (int i = 0; i < 256; i++) {
                
                for (int j = 0; j < 256; j++) {
                    mLUTImageA.set(i, j, fMin + ((float) i / 255.0f * (fMax - fMin)));
                }
            }
            mLUTImageA.calcMinMax();
            
            /* Create LUT */
            int[] dimExtentsLUT = { 4, 256 };
            mLUTSeparate = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
            mLUTSeparate.resetTransferLine(fMin, fMin, fMax, fMax);
            
            /* Remove old LUT if it exists: */
            if ( mHistoLUT != null )
            {
                mainPanel.remove( mHistoLUT.getMainPanel() );
                mHistoLUT = null;
            }
            /* Create LUT panel: */
            mHistoLUT = new JPanelHistoLUT( mLUTImageA, null, mLUTSeparate, null, true );
            mPatientSlice.setLUTa( mLUTSeparate );
        }
        else
        {
            float fMinR = (float)mImageA.getMinR();
            float fMaxR = (float)mImageA.getMaxR();

            float fMinG = (float)mImageA.getMinG();
            float fMaxG = (float)mImageA.getMaxG();

            float fMinB = (float)mImageA.getMinB();
            float fMaxB = (float)mImageA.getMaxB();

            int[] iExtents = { 256, 256 };
            mLUTImageA = new ModelImage(ModelStorageBase.ARGB_FLOAT, iExtents, "temp", ViewUserInterface.getReference());
            mLUTImageA.addImageDisplayListener(this);
            
            for (int j = 0; j < 256; j++) {
                for (int i = 0; i < 256; i++) {
                    mLUTImageA.setC(i, j, 0, 255.0f );
                    mLUTImageA.setC(i, j, 1, fMinR + ((float) j / 255.0f * (fMaxR - fMinR)));
                    mLUTImageA.setC(i, j, 2, fMinG + ((float) j / 255.0f * (fMaxG - fMinG)));
                    mLUTImageA.setC(i, j, 3, fMinB + ((float) j / 255.0f * (fMaxB - fMinB)));
                }
            }
            mLUTImageA.calcMinMax();

            /* Create LUT */
            int[] dimExtentsLUT = { 4, 256 };
            mRGBSeparate = new ModelRGB(dimExtentsLUT);            

            /* Remove old lut if it exists: */
            if ( mHistoRGB != null )
            {
                mainPanel.remove( mHistoRGB.getMainPanel() );
                mHistoRGB = null;
            }
            /* Create LUT panel: */
            mHistoRGB = new JPanelHistoRGB( mLUTImageA, null, mRGBSeparate, null, true );
            mPatientSlice.setRGBTA( mRGBSeparate );
        }

        if ( !mImageA.isColorImage() )
        {
            mainPanel.add( mHistoLUT.getMainPanel(), BorderLayout.SOUTH );
        }
        else
        {
            mainPanel.add( mHistoRGB.getMainPanel(), BorderLayout.SOUTH );
        }
        mainPanel.updateUI();
    }


    /** Enables or disables the interface. Called when a surface is
     * added/removed from the JPanelSurface class. 
     * @param flag, when true enable the interface, when false disable the
     * interface.
     */
    public void setEnabled( boolean flag )
    {
        mImageAsTextureRadioButton.setEnabled(flag);
        mImageAsVertexColorRadioButton.setEnabled(flag);
        mImageNoneRadioButton.setEnabled(flag);
        mModelLUTRadioButton.setEnabled(flag);
        mNewLUTRadioButton.setEnabled(flag);
    }

    /**
     * Returns whether the ModelTriangleMesh is to be displayed with the
     * ModelImage data as a texture or not.
     * @return true when the ModelImageMesh is texture-mapped, false otherwise.
     */
    public boolean getEnabled()
    {
        return (mTextureStatus == TEXTURE);
    }

    /**
     * Returns the texture status, either TEXTURE, VERTEX_COLOR, or NONE
     * @return TEXTURE, VERTEX_COLOR, or NONE
     */
    public int getTextureStatus()
    {
        return mTextureStatus;
    }

    /**
     * actionPerformed, listens for interface events.
     * @param event, ActionEvent generated by the interface.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        if (command.equals("LoadNewImage"))
        {
            JFileChooser chooser = new JFileChooser();
            chooser.setMultiSelectionEnabled(false);
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));

            if (ViewUserInterface.getReference().getDefaultDirectory() != null)
            {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            }
            else
            {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }
            if (JFileChooser.APPROVE_OPTION != chooser.showOpenDialog(null))
            {
                return;
            }

            mImageFileName = chooser.getSelectedFile().getName();
            mImageDirName = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            mImageFileNameLabel.setText(mImageFileName);
            chooser.setVisible(false);

            loadingImage();

        }
        else if (command.equals("ImageAsTexture"))
        {
            mTextureStatus = TEXTURE;
            if ( mModelLUTRadioButton.isSelected() )
            {
                updateImages( null, mLUTModel, false, 0 );
            }
            else
            {
                updateImages( mLUTSeparate, null, false, 0 );
            }
            ((SurfaceRender)renderBase).getSurfaceDialog().enableSurfacePaintCan( true );
        }
        else if (command.equals("ImageAsVertexColor"))
        {
            mTextureStatus = VERTEX_COLOR;
            ((SurfaceRender)renderBase).getSurfaceDialog().generateNewTextureCoords( mImageA, true, false );
            updateImages( null, null, false, 0 );
            ((SurfaceRender)renderBase).getSurfaceDialog().enableSurfacePaintCan( true );
        }
        else if (command.equals("ImageNone"))
        {
            mTextureStatus = NONE;
            updateImages( null, null, false, 0 );
            ((SurfaceRender)renderBase).getSurfaceDialog().restoreVertexColors();
            ((SurfaceRender)renderBase).getSurfaceDialog().enableSurfacePaintCan( false );
        }
        else if (command.equals("LinkLUTs"))
        {
            if ( !mImageA.isColorImage() )
            {
                mainPanel.remove( mHistoLUT.getMainPanel() );
            }
            else
            {
                mainPanel.remove( mHistoRGB.getMainPanel() );
            }
            mainPanel.updateUI();
            updateImages( null, mLUTModel, false, 0 );
        }
        else if (command.equals("SeparateLUTs"))
        {
            if ( !mImageA.isColorImage() )
            {
                mainPanel.add( mHistoLUT.getMainPanel(), BorderLayout.SOUTH );
            }
            else
            {
                mainPanel.add( mHistoRGB.getMainPanel(), BorderLayout.SOUTH );
            }
            mainPanel.updateUI();
            updateImages( mLUTSeparate, null, false, 0 );
        }
    }

    /**
     * Load a new ModelImage to use for the 3D Texture display:
     */
    private void loadingImage() {

        /* If the ModelImage m_kImage is not defined (null) or the name
         * doesn't equal the name set in m_kImageFile, then load the new
         * ModelImage: */
        if ((mImageA == null) || ((mImageA != null) && (!mImageFileName.equals(mImageA.getImageName()))))
        {
            FileIO fileIO = new FileIO();
            fileIO.setQuiet(true);

            mImageA = fileIO.readImage(mImageFileName, mImageDirName, false, null);
            mImageFileName = mImageA.getImageName();
            mImageFileNameLabel.setText( mImageFileName );
            initLUT();
            mainPanel.updateUI();

            mNewLUTRadioButton.setSelected(true);

            ((SurfaceRender)renderBase).getSurfaceDialog().generateNewTextureCoords( mImageA,
                                                                                     (mTextureStatus == VERTEX_COLOR), false );

            mPatientSlice = null;
            mPatientSlice = new PatientSlice( mImageA, null, null, null, FileInfoBase.UNKNOWN_ORIENT );
            updateImages( mLUTSeparate, null, false, 0 );
        }
    }


    /** Returns the mainPanel 
     * @return mainPanel;
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /** ViewImageUpdateInterface, unused here.
     * @param slice
     */
    public void setSlice(int slice) {}

    /** ViewImageUpdateInterface, unused here.
     * @param tSlice
     */
    public void setTimeSlice(int tSlice) {}

    /** ViewImageUpdateInterface, unused here.  */
    public boolean updateImageExtents() {
        return false;
    }

    /** ViewImageUpdateInterface, unused here.  */
    public boolean updateImages() {
        return false;
    }

    /** ViewImageUpdateInterface, unused here. 
     * @param flag
     */
    public boolean updateImages(boolean flag) {
        return false;
    }

    /**
     * updateImages, called when the ModelLUT changes. Updates the
     * ImageComponent3D object for Texture3D updates.  The ImageComponent3D
     * object is only updated if it is currently displayed.
     * @param LUTa, the LUT associated with the independent texture
     * @param LUTb, the LUT associated with the ModelImage
     * @param flag, forces update (ignored here)
     * @param interpMode (ignored here)
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
        if ( LUTa != null )
        {
            mLUTSeparate = LUTa;
            if ( !mModelLUTRadioButton.isSelected() )
            {
                mPatientSlice.setLUTa( mLUTSeparate );
            }
        }
        else if ( LUTb != null )
        {
            mLUTModel = LUTb;
            if ( mModelLUTRadioButton.isSelected() )
            {
                mPatientSlice.setLUTa( mLUTModel );
            }
        }
        updateTexture();
        return true;
    }

    /** Update the ModelRGB associated with the separate texture, and
     * regenerate the ImageComponente3D volume texture.
     * @param RGBTa, the new ModelRGB for the separate texture.
     */
    public void setRGBTA( ModelRGB RGBTa )
    {
        if ( RGBTa != null )
        {
            mRGBSeparate = RGBTa;
            if ( !mModelLUTRadioButton.isSelected() )
            {
                mPatientSlice.setRGBTA( mRGBSeparate );
                updateTexture();
            }
        }
    }

    /** Update the ModelRGB associated with the ModelImage texture, and
     * regenerate the ImageComponente3D volume texture.
     * @param RGBTa, the new ModelRGB for the ModelImage texture.
     */
    public void setRGBTB( ModelRGB RGBTb )
    {
        if ( RGBTb != null )
        {
            mRGBModel = RGBTb;
            if ( mModelLUTRadioButton.isSelected() )
            {
                mPatientSlice.setRGBTA( mRGBModel );
                updateTexture();
            }
        }
    }

    /**
     * Updaes the ImageComponent3D data if the texture display is turned
     * on. Notifies the JPanelSurface object of the update.
     */
    private void updateTexture()
    {
        /* Only update if the texture is displayed: */
        if ( mTextureStatus == TEXTURE )
        {
            mSurfaceTextureImage = generateVolumeTexture();
        }
        /* Report the update to JPanelSurface: */
        ActionEvent event = new ActionEvent( (Object)this, 0, "ImageAsTexture" );
        ((SurfaceRender)renderBase).getSurfaceDialog().actionPerformed( event );
    }

    /**
     * Returns the ModelImage associated with the independent LUT
     *
     * @return  the ModelImage associated with the independent LUT
     */
    public ModelImage getImageSeparate() {
        return mLUTImageA;
    }

    /**
     * Returns The SurfaceRender ModelImage imageA for linking to the LUT.
     *
     * @return mImageALink, for identifying the ModelLUT associated with mImageA.
     */
    public ModelImage getImageLink() {
        return mImageALink;
    }

    /**
     * Returns The ModelImage that is the data source for the Texture3D.
     *
     * @return mImageA, the ModelImage used to generate the Texture3D
     */
    public ModelImage getTextureImage() {
        return mImageA;
    }

    /** 
     * generates an ImageComponent3D containing the ModelImage data. Uses the
     * PatientSlice object to output the data into the ImageComponent3D, so
     * lut changes can be applied. The ImageComponent3D is used to create a
     * Texture3D object for texture-mapping the ModelImage data onto
     * ModelTriangleMesh objects.
     */
    private ImageComponent3D generateVolumeTexture()
    {
        if ( mImageA == null )
        {
            return null;
        }
        int buffFactor = mImageA.isColorImage() ? 4 : 1;
        int[] localExtents = mImageA.getExtents();
        int[] iImageBufferA = new int[ localExtents[0] * localExtents[1] * buffFactor ];
        int iNumberSlices = localExtents[2];
        ImageComponent3D kSurfaceTextureImage = new ImageComponent3D(ImageComponent.FORMAT_RGBA,
                                                                     localExtents[0], localExtents[1], localExtents[2],
                                                                     false, false);
        kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_IMAGE_WRITE);
        kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_IMAGE_READ);
        kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_SIZE_READ);
        kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_FORMAT_READ);
        BufferedImage mBuff = new BufferedImage( localExtents[0], localExtents[1], BufferedImage.TYPE_INT_ARGB );
        for (int iZ = 0; iZ < iNumberSlices; iZ++) {
            mPatientSlice.updateSlice( iZ );
            if ( mPatientSlice.showUsingOrientation( 0, iImageBufferA, null, true, false, 0, false ) )
            {
                mBuff.setRGB( 0, 0, localExtents[0], localExtents[1], iImageBufferA, 0, localExtents[0] );
                kSurfaceTextureImage.set( iZ, mBuff );
            }
        }
        return kSurfaceTextureImage;
    }

    /**
     * updateSurfaceTextureImage. Paints the ImageComponent3D object at the
     * location of the center parameter.  Note: this function is currently
     * unused because the J3D ImageComponent3D.setSubImage function does not
     * work.
     * @param center, the center-point of the TexturePaint operation.
     */
    public void updateSurfaceTextureImage( Point3f center )
    {
        /*
        if ( mImageA == null )
        {
            return;
        }
        int buffFactor = mImageA.isColorImage() ? 4 : 1;
        int[] localExtents = mImageA.getExtents();
        int iNumberSlices = localExtents[2];

        ImageComponent3D kSurfaceTextureImage = new ImageComponent3D(ImageComponent.FORMAT_RGBA,
                                                                     localExtents[0], localExtents[1], localExtents[2],
                                                                     false, false);
        kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_IMAGE_WRITE);
        kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_IMAGE_READ);
        kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_SIZE_READ);
        kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_FORMAT_READ);
        BufferedImage mBuff = new BufferedImage( localExtents[0], localExtents[1], BufferedImage.TYPE_INT_ARGB );

        int dstX = (int)(center.x - mKernelWidthHalf);
        if ( dstX < 0 )
        {
            dstX = 0;
        }
        if ( (dstX + mKernelWidthHalf ) > localExtents[0])
        {
            dstX = localExtents[0] - mKernelWidthHalf;
        }
        int dstY = (int)(center.y - mKernelHeightHalf);
        if ( dstY < 0 )
        {
            dstY = 0;
        }
        if ( (dstY + mKernelHeightHalf ) > localExtents[1])
        {
            dstY = localExtents[1] - mKernelHeightHalf;
        }
        
        kSurfaceTextureImage.set( mSurfaceTextureImage.getImage() );
        for ( int i = (int)(center.z-mKernelDepthHalf); i < (int)(center.z+mKernelDepthHalf); i++ )
        {
            if ( (i >= 0) && (i < iNumberSlices) )
            {
                mBuff = kSurfaceTextureImage.getImage( i );
                mBuff.setRGB( dstX, dstY, mKernelWidthHalf, mKernelHeightHalf, mKernel, 0, mKernelWidthHalf );
                //kSurfaceTextureImage.setSubImage( i, mKernel, 10, 10, 0, 0, dstX, dstY );
                kSurfaceTextureImage.set( i, mBuff );

            }
        } 
//         RenderedImage ri = test[0];
//         for ( int i = 0; i < iNumberSlices; i++ )
//         {
//             mSurfaceTextureImage.setSubImage( i, ri, localExtents[0], localExtents[1], 0, 0, 0, 0 );
//         }
        mSurfaceTextureImage = null;
        mSurfaceTextureImage = kSurfaceTextureImage;
        */
    }

    /**
     * updateSurfaceTextureImage. Paints the ImageComponent3D object with the paint mask. 
     * @param paintMask, paint bit map to add to the texture.
     */
    public void updateSurfaceTextureImage( int index, BitSet paintMask, Color4f kColor )
    {
        if ( mImageA == null )
        {
            return;
        }
        mImageA.addSurfaceMask( index, paintMask, null, kColor );

        if ( mTextureStatus == VERTEX_COLOR )
        {
            ((SurfaceRender)renderBase).getSurfaceDialog().generateNewTextureCoords( mImageA, true, true );
            updateImages( null, null, false, 0 );
        }
        else
        {
            int buffFactor = mImageA.isColorImage() ? 4 : 1;
            int[] localExtents = mImageA.getExtents();
            int[] iImageBufferA = new int[ localExtents[0] * localExtents[1] * buffFactor ];
            int iNumberSlices = localExtents[2];
            ImageComponent3D kSurfaceTextureImage = new ImageComponent3D(ImageComponent.FORMAT_RGBA,
                                                                         localExtents[0], localExtents[1], localExtents[2],
                                                                         false, false);
            kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_IMAGE_WRITE);
            kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_IMAGE_READ);
            kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_SIZE_READ);
            kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_FORMAT_READ);
            BufferedImage mBuff = new BufferedImage( localExtents[0], localExtents[1], BufferedImage.TYPE_INT_ARGB );
            for (int iZ = 0; iZ < iNumberSlices; iZ++) {
                mPatientSlice.updateSlice( iZ );
                if ( mPatientSlice.showUsingOrientation( 0, iImageBufferA, null, true, false, 0, true ) )
                {
                    mBuff.setRGB( 0, 0, localExtents[0], localExtents[1], iImageBufferA, 0, localExtents[0] );
                    kSurfaceTextureImage.set( iZ, mBuff );
                }
            }
            mSurfaceTextureImage = null;
            mSurfaceTextureImage = kSurfaceTextureImage;
        }
        /* Report the update to JPanelSurface: */
        ActionEvent event = new ActionEvent( (Object)this, 0, "ImageAsTexture" );
        ((SurfaceRender)renderBase).getSurfaceDialog().actionPerformed( event );
    }
}
