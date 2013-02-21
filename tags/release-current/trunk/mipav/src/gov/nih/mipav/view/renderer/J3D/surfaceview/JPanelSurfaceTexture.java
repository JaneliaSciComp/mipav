package gov.nih.mipav.view.renderer.J3D.surfaceview;


import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.J3D.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

import java.io.*;

import java.util.*;

import javax.media.j3d.*;

import javax.swing.*;

import javax.vecmath.*;


/**
 * JPanelSurfaceTexture. Enables texture-mapping of the ModelImage data onto a surface triangle mesh. The Texture
 * coordinates of the mesh are calculated in the SurfaceMask class. This class creates the ImageComponent3D object that
 * is passed to the Texture3D object when the surface display attributes (TextureUnitState) are created. The
 * ImageComponent3D object contained within this class updates when the user changes the ModelLUT associated with the
 * texture.
 *
 * <p>The user can change the ModelLUT independently of the ModelLUT associated with the ModelImage, or if the user
 * selects the option of using the ModelLUT associated with the ModelImage, then the texture updates as the user updates
 * that LUT. This class implements the ViewImageUpdateInterface to capture LUT changes.</p>
 *
 * @see  JPanelSurface.java
 * @see  SurfaceMask.java
 * @see  ModelImage.java
 */
public class JPanelSurfaceTexture extends JPanelRendererJ3D implements ViewImageUpdateInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7562070328632922435L;

    // Types of image-as-texture displays:
    /** No display:. */
    public static final int NONE = 0;

    /** Displays the ModelImage with 3D Texture-mapping :. */
    public static final int TEXTURE = 1;

    /** Displays the ModelImage with per-vertex colors:. */
    public static final int VERTEX_COLOR = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Display the independent LUT for Black/White images. */
    private JPanelHistoLUT mHistoLUT;

    /** Display the independent RGB for Color Images. */
    private JPanelHistoRGB mHistoRGB;

    /** ModelImage used to generate the 3D texture:. */
    private ModelImage mImageA;

    /** Reference to ModelImage A for linking the texture to the imageA LUT. */
    private ModelImage mImageALink;

    /** RadioButton for turing on the surface image texture:. */
    private JRadioButton mImageAsTextureRadioButton;

    /** RadioButton for turing on the surface image per-vertex color:. */
    private JRadioButton mImageAsVertexColorRadioButton;

    /** Stores the currently-loaded ModelImage directory name:. */
    private String mImageDirName;

    /** Stores the currently-loaded ModelImage file name:. */
    private String mImageFileName;

    /** Display the currently-loaded ModelImage file name:. */
    private JLabel mImageFileNameLabel;

    /** RadioButton for turing off the surface image:. */
    private JRadioButton mImageNoneRadioButton;

    // Interface buttons:
    /** Load a new ModelImage:. */
    private JButton mLoadImageButton;

    /** Grouping the radio buttons:. */
    private ButtonGroup mLUTButtonGroup = new ButtonGroup();

    /** Independent ModelImage for independent LUT. */
    private ModelImage mLUTImageA;

    /** The LUT associated with the ModelImage imageA:. */
    private ModelLUT mLUTModel = null;

    /** The LUT associated with the independent texture LUT:. */
    private ModelLUT mLUTSeparate = null;

    /** Use the ModelImage LUT. */
    private JRadioButton mModelLUTRadioButton;

    /** Use a separate LUT. */
    private JRadioButton mNewLUTRadioButton;

    /** Renders the ModelImage data with LUT changes. */
    private PatientSlice mPatientSlice = null;

    /** The RGB LUT associated with the ModelImage imageA:. */
    private ModelRGB mRGBModel = null;

    /** The RGB LUT associated with the independent texture LUT:. */
    private ModelRGB mRGBSeparate = null;

    /** The ModelImage texture with LUT changes, used with the Texture3D. */
    private ImageComponent3D mSurfaceTextureImage = null;

    /** 3D Texture used to display the ModelImage data as a texture mapped onto the ModelTriangleMesh. */
    private Texture3D mTexture = null;

    /** Grouping the radio buttons:. */
    private ButtonGroup mTextureButtonGroup = new ButtonGroup();

    /** Currently selected ModelImage display type:. */
    private int mTextureStatus = NONE;

    /** TextureUnitState contains texture information for the rendered surfaces:. */
    private TextureUnitState[] mTextureUnitState;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor:
     *
     * @param  kView   the SurfaceRender this panel is displayed with
     * @param  imageA  the ModelImage used to generate the volume texture.
     */
    public JPanelSurfaceTexture(SurfaceRender kView, ModelImage imageA) {
        super(kView);
        mImageA = imageA;
        mImageALink = imageA;
        mImageALink.addImageDisplayListener(this);
        mPatientSlice = new PatientSlice(mImageA, null, null, null, FileInfoBase.UNKNOWN_ORIENT);
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed, listens for interface events.
     *
     * @param  event  ActionEvent generated by the interface.
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

        } else if (command.equals("ImageAsTexture")) {
            mTextureStatus = TEXTURE;

            if (mModelLUTRadioButton.isSelected()) {
                updateImages(null, mLUTModel, false, 0);
            } else {
                updateImages(mLUTSeparate, null, false, 0);
            }

            ((SurfaceRender) renderBase).getSurfaceDialog().enableSurfacePaintCan(true);
            ((SurfaceRender) renderBase).getSurfaceDialog().enableSurfacePaint(false);
        } else if (command.equals("ImageAsVertexColor")) {
            mTextureStatus = VERTEX_COLOR;
            ((SurfaceRender) renderBase).getSurfaceDialog().generateNewTextureCoords(mImageA, true, false);
            ((SurfaceRender) renderBase).getSurfaceDialog().enableSurfacePaintCan(true);
            ((SurfaceRender) renderBase).getSurfaceDialog().enableSurfacePaint(true);
        } else if (command.equals("ImageNone")) {
            mTextureStatus = NONE;
            ((SurfaceRender) renderBase).getSurfaceDialog().restoreVertexColors();
            ((SurfaceRender) renderBase).getSurfaceDialog().enableSurfacePaintCan(false);
            ((SurfaceRender) renderBase).getSurfaceDialog().enableSurfacePaint(true);
        } else if (command.equals("LinkLUTs")) {

            if (!mImageA.isColorImage()) {
                mainPanel.remove(mHistoLUT.getMainPanel());
            } else {
                mainPanel.remove(mHistoRGB.getMainPanel());
            }

            mainPanel.updateUI();
            updateImages(null, mLUTModel, false, 0);
        } else if (command.equals("SeparateLUTs")) {

            if (!mImageA.isColorImage()) {
                mainPanel.add(mHistoLUT.getMainPanel(), BorderLayout.SOUTH);
            } else {
                mainPanel.add(mHistoRGB.getMainPanel(), BorderLayout.SOUTH);
            }

            mainPanel.updateUI();
            updateImages(mLUTSeparate, null, false, 0);
        }
    }

    /**
     * Removes this object from the ModelImage imageDisplayListener list.
     */
    public void dispose() {
        mImageALink.removeImageDisplayListener(this);

        if (mLUTImageA != null) {
            mLUTImageA.disposeLocal();
            mLUTImageA = null;
        }
    }

    /**
     * Returns whether the ModelTriangleMesh is to be displayed with the ModelImage data as a texture or not.
     *
     * @return  true when the ModelImageMesh is texture-mapped, false otherwise.
     */
    public boolean getEnabled() {
        return (mTextureStatus == TEXTURE);
    }

    /**
     * Returns The SurfaceRender ModelImage imageA for linking to the LUT.
     *
     * @return  mImageALink, for identifying the ModelLUT associated with mImageA.
     */
    public ModelImage getImageLink() {
        return mImageALink;
    }

    /**
     * Returns the ModelImage associated with the independent LUT.
     *
     * @return  the ModelImage associated with the independent LUT
     */
    public ModelImage getImageSeparate() {
        return mLUTImageA;
    }

    /**
     * Return the current ModelLUT:.
     *
     * @return  the currently used ModelLUT
     */
    public ModelLUT getLUT() {

        if (mModelLUTRadioButton.isSelected()) {
            return mLUTModel;
        }

        return mLUTSeparate;
    }


    /**
     * Returns the mainPanel.
     *
     * @return  mainPanel;
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Return the current ModelRGBT:.
     *
     * @return  the currently used ModelRGBT
     */
    public ModelRGB getRGBT() {

        if (mModelLUTRadioButton.isSelected()) {
            return mRGBModel;
        }

        return mRGBSeparate;
    }

    /**
     * Returns The ModelImage that is the data source for the Texture3D.
     *
     * @return  mImageA, the ModelImage used to generate the Texture3D
     */
    public ModelImage getTextureImage() {
        return mImageA;
    }

    /**
     * Returns the texture status, either TEXTURE, VERTEX_COLOR, or NONE.
     *
     * @return  TEXTURE, VERTEX_COLOR, or NONE
     */
    public int getTextureStatus() {
        return mTextureStatus;
    }


    /**
     * Returns the volume texture TextureUnitState.
     *
     * @return  mTextureUnitState, the volume texture data.
     */
    public TextureUnitState[] getTextureUnitState() {
        return mTextureUnitState;
    }


    /**
     * Enables or disables the interface. Called when a surface is added/removed from the JPanelSurface class.
     *
     * @param  flag  when true enable the interface, when false disable the interface.
     */
    public void setEnabled(boolean flag) {
        mImageAsTextureRadioButton.setEnabled(flag);
        mImageAsVertexColorRadioButton.setEnabled(flag);
        mImageNoneRadioButton.setEnabled(flag);
        mModelLUTRadioButton.setEnabled(flag);
        mNewLUTRadioButton.setEnabled(flag);
    }

    /**
     * Update the ModelRGB associated with the separate texture, and regenerate the ImageComponente3D volume texture.
     *
     * @param  RGBTa  the new ModelRGB for the separate texture.
     */
    public void setRGBTA(ModelRGB RGBTa) {

        if (RGBTa != null) {
            mRGBSeparate = RGBTa;

            if (!mModelLUTRadioButton.isSelected()) {
                mPatientSlice.setRGBTA(mRGBSeparate);
                updateTexture();
            }
        }
    }

    /**
     * Update the ModelRGB associated with the ModelImage texture, and regenerate the ImageComponente3D volume texture.
     *
     * @param  RGBTb  the new ModelRGB for the ModelImage texture.
     */
    public void setRGBTB(ModelRGB RGBTb) {

        if (RGBTb != null) {
            mRGBModel = RGBTb;

            if (mModelLUTRadioButton.isSelected()) {
                mPatientSlice.setRGBTA(mRGBModel);
                updateTexture();
            }
        }
    }

    /**
     * ViewImageUpdateInterface, unused here.
     *
     * @param  slice  DOCUMENT ME!
     */
    public void setSlice(int slice) { }

    /**
     * ViewImageUpdateInterface, unused here.
     *
     * @param  tSlice  DOCUMENT ME!
     */
    public void setTimeSlice(int tSlice) { }

    /**
     * ViewImageUpdateInterface, unused here.
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     * ViewImageUpdateInterface, unused here.
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImages() {
        return false;
    }

    /**
     * ViewImageUpdateInterface, unused here.
     *
     * @param   flag  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImages(boolean flag) {
        return false;
    }

    /**
     * updateImages, called when the ModelLUT changes. Updates the ImageComponent3D object for Texture3D updates. The
     * ImageComponent3D object is only updated if it is currently displayed.
     *
     * @param   LUTa        the LUT associated with the independent texture
     * @param   LUTb        the LUT associated with the ModelImage
     * @param   flag        forces update (ignored here)
     * @param   interpMode  (ignored here)
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {

        if (LUTa != null) {
            mLUTSeparate = LUTa;

            if (!mModelLUTRadioButton.isSelected()) {
                mPatientSlice.setLUTa(mLUTSeparate);
            }
        } else if (LUTb != null) {
            mLUTModel = LUTb;

            if (mModelLUTRadioButton.isSelected()) {
                mPatientSlice.setLUTa(mLUTModel);
            }
        }

        updateTexture();

        return true;
    }

    /**
     * updateSurfaceTextureImage. Paints the ImageComponent3D object with the paint mask.
     *
     * @param  paintMask  paint bit map to add to the texture.
     * @param  kColor     DOCUMENT ME!
     */
    public void updateSurfaceTextureImage(BitSet paintMask, Color4f kColor) {

        if (mImageA == null) {
            return;
        }

        boolean bUsePaintMask = false;
        BitSet[] saveMasks = mImageA.removeSurfaceMasks();

        if (paintMask != null) {
            bUsePaintMask = true;
            mImageA.addSurfaceMask(0, paintMask, null, new ColorRGBA( kColor.x, kColor.y, kColor.z, kColor.w ));
        }

        if (mTextureStatus == VERTEX_COLOR) {
            ((SurfaceRender) renderBase).getSurfaceDialog().generateNewTextureCoords(mImageA, true, bUsePaintMask);
        } else {
            int buffFactor = mImageA.isColorImage() ? 4 : 1;
            int[] localExtents = mImageA.getExtents();
            int[] iImageBufferA = new int[localExtents[0] * localExtents[1] * buffFactor];
            int iNumberSlices = localExtents[2];
            ImageComponent3D kSurfaceTextureImage = new ImageComponent3D(ImageComponent.FORMAT_RGBA, localExtents[0],
                                                                         localExtents[1], localExtents[2], false,
                                                                         false);
            kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_IMAGE_WRITE);
            kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_IMAGE_READ);
            kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_SIZE_READ);
            kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_FORMAT_READ);

            BufferedImage mBuff = new BufferedImage(localExtents[0], localExtents[1], BufferedImage.TYPE_INT_ARGB);

            for (int iZ = 0; iZ < iNumberSlices; iZ++) {
                mPatientSlice.updateSlice(iZ);

                if (mPatientSlice.showUsingOrientation(0, iImageBufferA, null, true, false, 0, bUsePaintMask)) {
                    mBuff.setRGB(0, 0, localExtents[0], localExtents[1], iImageBufferA, 0, localExtents[0]);
                    kSurfaceTextureImage.set(iZ, mBuff);
                }
            }

            mSurfaceTextureImage = null;
            mSurfaceTextureImage = kSurfaceTextureImage;

            mTexture.setImage(0, mSurfaceTextureImage);
        }

        mImageA.restoreSurfaceMasks(saveMasks);
    }

    /**
     * Creates a 3D Texture object for the ModelImage displayed in the SurfaceRender object. The Texture3D object is
     * used for texture-mapping on any or all ModelTriangleMesh objects displayed in the SurfaceRender.
     */
    private void createVolumeTexture() {

        int[] localExtents = mImageA.getExtents();

        mTexture = new Texture3D(Texture.BASE_LEVEL, Texture.RGBA, localExtents[0], localExtents[1], localExtents[2]);
        mTexture.setEnable(true);
        mTexture.setMinFilter(Texture.BASE_LEVEL_LINEAR);
        mTexture.setMagFilter(Texture.NICEST);

        // m_kTexture.setAnisotropicFilterMode(Texture.ANISOTROPIC_SINGLE_VALUE);
        // m_kTexture.setAnisotropicFilterDegree(5);
        mTexture.setBoundaryModeS(Texture.CLAMP_TO_BOUNDARY);
        mTexture.setBoundaryModeT(Texture.CLAMP_TO_BOUNDARY);
        mTexture.setBoundaryModeR(Texture.CLAMP_TO_BOUNDARY);
        mTexture.setBoundaryColor(0.0f, 0.0f, 0.0f, 0.0f);
        mTexture.setImage(0, mSurfaceTextureImage);
        mTexture.setCapability(Texture3D.ALLOW_IMAGE_WRITE);
        mTexture.setCapability(Texture3D.ALLOW_IMAGE_READ);
        mTexture.setCapability(Texture3D.ALLOW_ENABLE_WRITE);

        // Texture Attributes:
        TextureAttributes kTextureAttr = new TextureAttributes();
        kTextureAttr.setTextureMode(TextureAttributes.REPLACE);

        // Texture Unit State:
        mTextureUnitState = new TextureUnitState[1];
        mTextureUnitState[0] = new TextureUnitState(mTexture, kTextureAttr, null);
        mTextureUnitState[0].setCapability(TextureUnitState.ALLOW_STATE_WRITE);
    }

    /**
     * generates an ImageComponent3D containing the ModelImage data. Uses the PatientSlice object to output the data
     * into the ImageComponent3D, so lut changes can be applied. The ImageComponent3D is used to create a Texture3D
     * object for texture-mapping the ModelImage data onto ModelTriangleMesh objects.
     *
     * @return  DOCUMENT ME!
     */
    private ImageComponent3D generateVolumeTexture() {

        if (mImageA == null) {
            return null;
        }

        int buffFactor = mImageA.isColorImage() ? 4 : 1;
        int[] localExtents = mImageA.getExtents();
        int[] iImageBufferA = new int[localExtents[0] * localExtents[1] * buffFactor];
        int iNumberSlices = localExtents[2];
        ImageComponent3D kSurfaceTextureImage = new ImageComponent3D(ImageComponent.FORMAT_RGBA, localExtents[0],
                                                                     localExtents[1], localExtents[2], false, false);
        kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_IMAGE_WRITE);
        kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_IMAGE_READ);
        kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_SIZE_READ);
        kSurfaceTextureImage.setCapability(ImageComponent3D.ALLOW_FORMAT_READ);

        BufferedImage mBuff = new BufferedImage(localExtents[0], localExtents[1], BufferedImage.TYPE_INT_ARGB);

        for (int iZ = 0; iZ < iNumberSlices; iZ++) {
            mPatientSlice.updateSlice(iZ);

            if (mPatientSlice.showUsingOrientation(0, iImageBufferA, null, true, false, 0, false)) {
                mBuff.setRGB(0, 0, localExtents[0], localExtents[1], iImageBufferA, 0, localExtents[0]);
                kSurfaceTextureImage.set(iZ, mBuff);
            }
        }

        return kSurfaceTextureImage;
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
        mImageFileName = mImageA.getImageName();
        mImageFileNameLabel.setText(mImageFileName);
        imagePanel.add(mImageFileNameLabel, gbc);


        JPanel texturePanel = new JPanel();
        texturePanel.setBorder(buildTitledBorder("Image display options"));
        texturePanel.setLayout(new GridBagLayout());
        texturePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        gbc.gridx = 0;
        gbc.gridy = 0;

        mImageAsTextureRadioButton = new JRadioButton("3D Texture");
        mImageAsTextureRadioButton.addActionListener(this);
        mImageAsTextureRadioButton.setActionCommand("ImageAsTexture");
        mImageAsTextureRadioButton.setSelected(false);
        mImageAsTextureRadioButton.setEnabled(false);
        mTextureButtonGroup.add(mImageAsTextureRadioButton);
        texturePanel.add(mImageAsTextureRadioButton, gbc);

        gbc.gridx++;
        mImageAsVertexColorRadioButton = new JRadioButton("Per-Vertex Color");
        mImageAsVertexColorRadioButton.addActionListener(this);
        mImageAsVertexColorRadioButton.setActionCommand("ImageAsVertexColor");
        mImageAsVertexColorRadioButton.setSelected(false);
        mImageAsVertexColorRadioButton.setEnabled(false);
        mTextureButtonGroup.add(mImageAsVertexColorRadioButton);
        texturePanel.add(mImageAsVertexColorRadioButton, gbc);

        gbc.gridx++;
        mImageNoneRadioButton = new JRadioButton("None");
        mImageNoneRadioButton.addActionListener(this);
        mImageNoneRadioButton.setActionCommand("ImageNone");
        mImageNoneRadioButton.setSelected(true);
        mImageNoneRadioButton.setEnabled(false);
        mTextureButtonGroup.add(mImageNoneRadioButton);
        texturePanel.add(mImageNoneRadioButton, gbc);


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

        mSurfaceTextureImage = generateVolumeTexture();
        createVolumeTexture();
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
                    mLUTImageA.set(i, j, fMin + ((float) i / 255.0f * (fMax - fMin)));
                }
            }

            mLUTImageA.calcMinMax();

            /* Create LUT */
            int[] dimExtentsLUT = { 4, 256 };
            mLUTSeparate = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
            mLUTSeparate.resetTransferLine(fMin, fMin, fMax, fMax);

            /* Remove old LUT if it exists: */
            if (mHistoLUT != null) {
                mainPanel.remove(mHistoLUT.getMainPanel());
                mHistoLUT = null;
            }

            /* Create LUT panel: */
            mHistoLUT = new JPanelHistoLUT(mLUTImageA, null, mLUTSeparate, null, true);
            mPatientSlice.setLUTa(mLUTSeparate);
        } else {
            float fMinR = (float) mImageA.getMinR();
            float fMaxR = (float) mImageA.getMaxR();

            float fMinG = (float) mImageA.getMinG();
            float fMaxG = (float) mImageA.getMaxG();

            float fMinB = (float) mImageA.getMinB();
            float fMaxB = (float) mImageA.getMaxB();

            int[] iExtents = { 256, 256 };
            mLUTImageA = new ModelImage(ModelStorageBase.ARGB_FLOAT, iExtents, "temp");
            mLUTImageA.addImageDisplayListener(this);

            for (int j = 0; j < 256; j++) {

                for (int i = 0; i < 256; i++) {
                    mLUTImageA.setC(i, j, 0, 255.0f);
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
            if (mHistoRGB != null) {
                mainPanel.remove(mHistoRGB.getMainPanel());
                mHistoRGB = null;
            }

            /* Create LUT panel: */
            mHistoRGB = new JPanelHistoRGB(mLUTImageA, null, mRGBSeparate, null, true);
            mPatientSlice.setRGBTA(mRGBSeparate);
        }

        if (!mImageA.isColorImage()) {
            mainPanel.add(mHistoLUT.getMainPanel(), BorderLayout.SOUTH);
        } else {
            mainPanel.add(mHistoRGB.getMainPanel(), BorderLayout.SOUTH);
        }

        mainPanel.updateUI();
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
            mImageFileName = mImageA.getImageName();
            mImageFileNameLabel.setText(mImageFileName);
            initLUT();
            mainPanel.updateUI();

            mNewLUTRadioButton.setSelected(true);

            ((SurfaceRender) renderBase).getSurfaceDialog().generateNewTextureCoords(mImageA,
                                                                                     (mTextureStatus == VERTEX_COLOR),
                                                                                     false);

            mPatientSlice = null;
            mPatientSlice = new PatientSlice(mImageA, null, null, null, FileInfoBase.UNKNOWN_ORIENT);
            updateImages(mLUTSeparate, null, false, 0);
        }
    }

    /**
     * Updaes the ImageComponent3D data if the texture display is turned on. Notifies the JPanelSurface object of the
     * update.
     */
    private void updateTexture() {

        /* Only update if the texture is displayed: */
        if (mTextureStatus == TEXTURE) {
            mSurfaceTextureImage = generateVolumeTexture();
            mTexture.setImage(0, mSurfaceTextureImage);

            /* Report the update to JPanelSurface: */
            ActionEvent event = new ActionEvent((Object) this, 0, "ImageAsTexture");
            ((SurfaceRender) renderBase).getSurfaceDialog().actionPerformed(event);
        }
        /* Only update if the texture is displayed: */
        else if (mTextureStatus == VERTEX_COLOR) {
            ((SurfaceRender) renderBase).getSurfaceDialog().generateNewTextureCoords(mImageA, true, false);
        }
    }
}
