package gov.nih.mipav.view.renderer.J3D.surfaceview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;

import com.sun.j3d.utils.universe.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

import javax.media.j3d.*;

import javax.swing.*;
import javax.swing.event.*;

import javax.vecmath.*;


/**
 * Class JStereoWindow:
 *
 * <p>Creates a stereo viewing window for both ModelTriangleMesh surfaces and arbitrary Group Nodes, including the
 * NodeVolumeTextureRender. Objects are presented on three canvases: left-eye view, right-eye view, and a anaglyph color
 * view. The user can change the eye separation with a slider that adjusts the focal length.</p>
 *
 * <p>The stereo rendering algorithm for creating the left and right-eye viewing transformations and anaglyph display is
 * based on Paul Bourke's web information at http://astronomy.swin.edu.au/~pbourke/stereographics/</p>
 *
 * @author  Alexandra Bokinsky
 */
public class JStereoWindow extends JFrame
        implements WindowListener, ActionListener, ChangeListener, ComponentListener,
                   MouseMotionListener /* mouse moved events */ {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2284771559648676504L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Three SimpleUniverses, one for each canavas:. */
    private SimpleUniverse[] m_akUniverse = new SimpleUniverse[3];

    /** Original eye separation, read from the canvas.view.physicalbody. */
    private double m_dEyeSeparation;

    /** The current universe to be initialized:. */
    private int m_iCurrentUniverse = 0;

    /** First time the stereo canvas is rendered, set to true:. */
    private int m_iStereoUpdateCount = 0;

    /** BufferedImage Dimensions (power of two). */
    private Dimension m_kD;

    /** The JSplitPane displaying the left and right eye views:. */
    private JSplitPane m_kDisplayPane;

    /** Focal-length, (eye separation) slider. */
    private JSlider m_kEyeSep_Slider;

    /** Left-eye view canvas:. */
    private VolumeCanvas3D m_kLeftCanvas;

    /** Left Image Panel, holds the canvas for the left-eye view:. */
    private JPanel m_kLeftImagePanel;

    /** The JSplitPane displaying different views and the user-interface:. */
    private JSplitPane m_kMainPane;

    /** BufferedImage containing the Anagyphy stereo image:. */
    private BufferedImage m_kRGStereo;

    /** Right-eye view canvas:. */
    private VolumeCanvas3D m_kRightCanvas;

    /** Right Image Panel, holds the canvas for the right-eye view:. */
    private JPanel m_kRightImagePanel;

    /** Stereo canvas:. */
    private VolumeCanvas3D m_kStereoCanvas;

    /** The JSplitPane displaying the left and right eye views and the stereo view:. */
    private JSplitPane m_kStereoPane;

    /** Stereo Image Panel, holds the canvas for the Anaglyph stereo view:. */
    private JPanel m_kStereoPanel;

    /** Texture-mapped polygon for displaying the Anaglyph stereo image as a texture:. */
    private Shape3D m_kStereoShape;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a JStereoWindow with a ModelTriangleMesh and Transform3D representing the model transformation for the
     * surface.
     *
     * @param  kMesh       the displayed surface
     * @param  kTransform  the current model transformation (rotation, scale, translation) for the surface.
     */
    public JStereoWindow( SurfaceAttributes kSurface, Transform3D kTransform) {

        /* Create the window: */
        super("StereoView");

        ModelTriangleMesh[] kMeshes = kSurface.getMesh();
        BranchGroup kLeft = new BranchGroup();
        BranchGroup kRight = new BranchGroup();
        for ( int i = 0; i < kMeshes.length; i++ )
        {
            ModelTriangleMesh kMesh = new ModelTriangleMesh(kMeshes[i].getVertexCopy(),
                                                            kMeshes[i].getNormalCopy(),
                                                            kMeshes[i].getIndexCopy()  );

            /* The left and right-eye views are displayed in grayscale so the
             * anaglyph view can be created using the two frame buffers: */
            Appearance kAppearance = new Appearance();
            kAppearance.setMaterial(new Material(new Color3f(0f, 0f, 0f), new Color3f(0f, 0f, 0f),
                                                 new Color3f(.8f, .8f, .8f), new Color3f(0f, 0f, 0f), 100f));
            kAppearance.getMaterial().setLightingEnable(true);

            TransformGroup kTransformGroupLeft = new TransformGroup(new Transform3D());
            kTransformGroupLeft.addChild( addBranchGroup(new Shape3D(kMesh, kAppearance), kTransform) );
            kLeft.addChild(kTransformGroupLeft);

            TransformGroup kTransformGroupRight = new TransformGroup(new Transform3D());
            kTransformGroupRight.addChild( addBranchGroup(new Shape3D(kMesh, kAppearance), kTransform) );
            kRight.addChild(kTransformGroupRight);
        }
        /* Initialize and create the scene graphs and canvases: */
        init( kLeft, kRight );

        /* Setup the user-interface: */
        setupInterface();

        /* Listen for the close window event: */
        addWindowListener(this);
        addComponentListener(this);
    }

    /**
     * Constructs a JStereoWindow with two nodes, one copy for each of the left and right eye views. The input nodes are
     * identical. The Transform3D represents the model transformation for the nodes.
     *
     * @param  kNode1      1st copy of the displayed object (NodeVolumeTextureRender)
     * @param  kNode2      2nd copy of the displayed object (NodeVolumeTextureRender)
     * @param  kTransform  the current model transformation (rotation, scale, translation) for the nodes.
     * @param  kSurface    the parent window.
     */
    public JStereoWindow(Group kNode1, Group kNode2, Transform3D kTransform, SurfaceRender kSurface) {

        /* Create the window: */
        super("StereoView");

        /* Initialize and create the scene graphs and canvases: */
        init(addBranchGroup(kNode1, kTransform), addBranchGroup(kNode2, kTransform));

        /* Setup the user-interface: */
        setupInterface();

        /* Listen for the close window event: */
        addWindowListener(this);
        addComponentListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Called when a button is pressed:
     *
     * @param  kEvent  action event
     */
    public void actionPerformed(ActionEvent kEvent) {
        String kCommand = kEvent.getActionCommand();

        /* OK Button in the JStereoWindow interface is pressed: close this
         * window: */
        if (kCommand.equals("OK_CLOSE")) {
            setVisible(false);
            this.dispose();
        } else if (kCommand.equals("UpdateStereo")) {
            updateStereo();
        }
    }


    /**
     * componentHidden.
     *
     * @param  e  ComponentEvent
     */
    public void componentHidden(ComponentEvent e) {
        // System.err.println("componentHidden");
    }

    /**
     * componentMoved.
     *
     * @param  e  ComponentEvent
     */
    public void componentMoved(ComponentEvent e) {
        // System.err.println("componentMoved");
    }

    /**
     * componentResized.
     *
     * @param  e  ComponentEvent
     */
    public void componentResized(ComponentEvent e) {

        // System.err.println("componentResized");
        m_kDisplayPane.setDividerLocation(0.5);
        m_kStereoPane.setDividerLocation(0.5);
        m_kMainPane.setDividerLocation(0.85);
    }

    /**
     * componentShown.
     *
     * @param  e  ComponentEvent
     */
    public void componentShown(ComponentEvent e) {
        // System.err.println("componentShown");
    }

    /**
     * Deletes all local data members.
     */
    public void dispose() {
        m_kLeftImagePanel.removeAll();
        m_kLeftImagePanel = null;
        m_kRightImagePanel.removeAll();
        m_kRightImagePanel = null;
        m_kStereoPanel.removeAll();
        m_kStereoPanel = null;

        m_kDisplayPane = null;
        m_kStereoPane = null;
        m_kMainPane = null;

        m_kEyeSep_Slider = null;

        m_kLeftCanvas = null;
        m_kRightCanvas = null;
        m_kStereoCanvas = null;
        m_kRGStereo = null;
        m_kD = null;

        m_kStereoShape = null;

        for (int iUniverse = 0; iUniverse < m_iCurrentUniverse; iUniverse++) {
            m_akUniverse[iUniverse].removeAllLocales();
            m_akUniverse[iUniverse].cleanup();
            m_akUniverse[iUniverse] = null;
        }

        m_akUniverse = null;

        super.dispose();
        System.gc();
    }

    /**
     * mouseDragged.
     *
     * @param  e  MouseEvent
     */
    public void mouseDragged(MouseEvent e) {
        // System.err.println("mouseDragged");
    }


    /**
     * mouseMoved.
     *
     * @param  e  MouseEvent
     */
    public void mouseMoved(MouseEvent e) {

        // System.err.println("mouseMoved");
        if (m_iStereoUpdateCount++ == 10) {
            updateStereo();
            m_kLeftCanvas.removeMouseMotionListener(this);
            m_kRightCanvas.removeMouseMotionListener(this);
            m_kStereoCanvas.removeMouseMotionListener(this);
        }
    }

    /**
     * Called when the slider values have changed. Reads the new focal-length value and recalculates the viewing
     * transformations for the left and right-eye views.
     *
     * @param  event  ChangeEvent state change event.
     */
    public void stateChanged(ChangeEvent event) {

        if (event.getSource() == m_kEyeSep_Slider) {
            float fEyeSep = (float) (m_kEyeSep_Slider.getValue()) / 2000f;
            Point3d kLeftEye = new Point3d(-fEyeSep, 0, 0);
            m_kLeftCanvas.getView().getPhysicalBody().setLeftEyePosition(kLeftEye);

            Point3d kRightEye = new Point3d(fEyeSep, 0, 0);
            m_kRightCanvas.getView().getPhysicalBody().setRightEyePosition(kRightEye);

            if (!m_kEyeSep_Slider.getValueIsAdjusting()) {
                updateStereo();
            }
        }
    }

    /**
     * windowActivated.
     *
     * @param  e  WindowEvent
     */
    public void windowActivated(WindowEvent e) {
        // System.err.println("windowActivated");
    }

    /**
     * windowClosed.
     *
     * @param  e  WindowEvent
     */
    public void windowClosed(WindowEvent e) {
        // System.err.println("windowClosed");
    }

    /**
     * windowClosing Catch the windowClosing event so that the local data memebrs can be destroyed.
     *
     * @param  e  WindowEvent
     */
    public void windowClosing(WindowEvent e) {

        // System.err.println("windowClosing");
        this.dispose();
    }

    /**
     * windowDeactivated.
     *
     * @param  e  WindowEvent
     */
    public void windowDeactivated(WindowEvent e) {
        // System.err.println("windowDeactivated");
    }

    /**
     * windowDeiconified.
     *
     * @param  e  WindowEvent
     */
    public void windowDeiconified(WindowEvent e) {

        // System.err.println("windowDeiconified");
        m_kDisplayPane.setDividerLocation(0.5);
        m_kStereoPane.setDividerLocation(0.5);
        m_kMainPane.setDividerLocation(0.85);
    }

    /**
     * windowIconified.
     *
     * @param  e  WindowEvent
     */
    public void windowIconified(WindowEvent e) {
        // System.err.println("windowIconified");
    }

    /**
     * windowOpened.
     *
     * @param  e  WindowEvent
     */
    public void windowOpened(WindowEvent e) {
        // System.err.println("windowOpened");
    }

    /**
     * Creates a BranchGroup containing the input Node and Transform3D, and returns the BranchGroup.
     *
     * @param   kNode       the Node to add to the BranchGroup
     * @param   kTransform  the transform for the input Node
     *
     * @return  the Branch of the SceneGraph containing the node and transform.
     */
    private BranchGroup addBranchGroup(Node kNode, Transform3D kTransform) {
        BranchGroup kBranch = new BranchGroup();
        TransformGroup kTransformGroup = new TransformGroup(new Transform3D(kTransform));
        kTransformGroup.addChild(kNode);
        kBranch.addChild(kTransformGroup);

        return kBranch;
    }

    /**
     * Blend two BufferedImages, representing the left and right-eye views into a Red-Cyan stereo anaglyph.
     *
     * @param  kLeft   the frame buffer from the left-eye rendering.
     * @param  kRight  the frame buffer from the right-eye rendering. This function does not return a value, but it sets
     *                 the BufferedImage member variable m_kStereo and the Dimension member variable m_kD.
     */
    private void blendBuffers(BufferedImage kLeft, BufferedImage kRight) {

        /* Determine the sizes of each BufferedImage, they should be the same,
         * but if not, choose the smallest size: */
        int iWidthL = kLeft.getWidth();
        int iHeightL = kLeft.getHeight();
        int iWidthR = kRight.getWidth();
        int iHeightR = kRight.getHeight();
        int iWidth = iWidthL;
        int iHeight = iHeightL;

        if (iWidth > iWidthR) {
            iWidth = iWidthR;
        }

        if (iHeight > iHeightR) {
            iHeight = iHeightR;
        }

        /* Determine the next-largest size that is a power of two, to create
         * the texture: */
        double dLog2Width = Math.log((double) iWidth) / Math.log(2.0);
        double dLog2Height = Math.log((double) iHeight) / Math.log(2.0);
        int iWidthPow2 = (int) Math.pow(2, Math.ceil(dLog2Width));
        int iHeightPow2 = (int) Math.pow(2, Math.ceil(dLog2Height));

        /* If the input BufferedImages are not the same size as the power of
         * two image, determine an offset so that the input images are
         * centered in the new image: */
        int iXOffset = (iWidthPow2 - iWidth) / 2;
        int iYOffset = (iHeightPow2 - iHeight) / 2;

        /* Set the dimension of the output image: */
        m_kD.width = iWidthPow2;
        m_kD.height = iHeightPow2;

        /* Create the new BufferedImage, to be used as the Texture: */
        m_kRGStereo = new BufferedImage(iWidthPow2, iHeightPow2, kLeft.getType());

        /* Loop over the input left and right buffers, blending the colors for
         * the anaglyph: */
        int iLeftC, iRightC, iBlendC;

        for (int iX = 0; iX < iWidth; iX++) {

            for (int iY = 0; iY < iHeight; iY++) {
                iLeftC = kLeft.getRGB(iX, iY);
                iRightC = kRight.getRGB(iX, iY);
                iBlendC = blendColor(iLeftC, iRightC);
                m_kRGStereo.setRGB(iXOffset + iX, iYOffset + iY, iBlendC);
            }
        }
    }


    /**
     * Blends two packed-integer colors (RGB) to create a Red-Cyan anaglyph.
     *
     * @param   iLeftC   the color from the left-eye view.
     * @param   iRightC  the color from the right-eye view.
     *
     * @return  the anaglyph color (packed-int) with the red component from the left-eye view and the green and blue
     *          components from the right-eye view, creating a Red-Cyan stereo.
     */
    private int blendColor(int iLeftC, int iRightC) {

        /* Unpack the left color into separate RGB components: */
        int iLeftRed = ((iLeftC >> 16) & 0xFF);
        //int iLeftGreen = ((iLeftC >> 8) & 0xFF);
        //int iLeftBlue = (iLeftC & 0xFF);

        /* Unpack the Right color into separate RGB components: */
        //int iRightRed = ((iRightC >> 16) & 0xFF);
        int iRightGreen = ((iRightC >> 8) & 0xFF);
        int iRightBlue = (iRightC & 0xFF);

        /* Create Red-Cyan anaglyph: using the red component from the left-eye
         * view and the green and blue color components from the right-eye
         * view: */
        int iRed = iLeftRed;
        int iGreen = iRightGreen;
        int iBlue = iRightBlue;

        /* Pack and return the new color: */
        int iNewColor = (iRed << 16) | (iGreen << 8) | (iBlue) | (0xff000000);

        return iNewColor;
    }

    /**
     * Creates a canvas containing the input Node object, with the input viewing transformation. The canvas is displayed
     * in the input JPanel:
     *
     * @param   kNode   the object displayed in this canvas.
     * @param   kPanel  the window panel containing this canvas.
     *
     * @return  the new canvas.
     */
    private VolumeCanvas3D createCanvas(Node kNode, JPanel kPanel) {

        /* Create the scene-graph for this canvas: */
        BranchGroup kScene = new BranchGroup();
        kScene.addChild(kNode);

        /* Light the scene: */
        Color3f lightColor = new Color3f(1, 1, 1);
        Vector3f lightDir = new Vector3f(-.5f, -.5f, -1f);
        DirectionalLight light = new DirectionalLight(lightColor, lightDir);
        light.setInfluencingBounds(new BoundingSphere(new Point3d(0.0f, 0.0f, 0.0f), 100.0f));
        kScene.addChild(light);

        /* Create the VolumeCanvas3D and SimpleUniverse: */
        GraphicsConfiguration kConfig = SimpleUniverse.getPreferredConfiguration();
        VolumeCanvas3D kCanvas = new VolumeCanvas3D(kConfig);
        kCanvas.addMouseMotionListener(this);
        m_akUniverse[m_iCurrentUniverse] = new SimpleUniverse(kCanvas);
        m_akUniverse[m_iCurrentUniverse].getViewingPlatform().setNominalViewingTransform();
        m_akUniverse[m_iCurrentUniverse].addBranchGraph(kScene);
        m_iCurrentUniverse++;

        kPanel.add(kCanvas, BorderLayout.CENTER);
        kPanel.setMinimumSize(new Dimension(600, 300));
        kPanel.setMaximumSize(new Dimension(600, 300));

        return kCanvas;
    }


    /**
     * Creates the texture-mapped polygon displayed in the stereo canvas. The left and right-eye views are read from the
     * left and right-eye canvases and the images blended into a stereo view. The blended image is used as a texture and
     * displayed on the texture-mapped polygon created in the function:
     */
    private void createTexturedPolygon() {

        /* Setup appearance attributes for the texture mapped polygon. */
        Appearance kDefaultAppearance = new Appearance();

        /* Disable lighting so that the color information comes from the
         *texture maps. */
        Material kMaterial = new Material();
        kMaterial.setLightingEnable(false);
        kDefaultAppearance.setMaterial(kMaterial);

        /* Use Replace mode so all color comes from the texture. */
        TextureAttributes kTextureAttr = new TextureAttributes();
        kTextureAttr.setTextureMode(TextureAttributes.REPLACE);
        kDefaultAppearance.setTextureAttributes(kTextureAttr);

        /* Allow the Texture to be modified. */
        kDefaultAppearance.setCapability(Appearance.ALLOW_TEXTURE_WRITE);

        /* The texture-mapped polygon geometry: */
        QuadArray kGeometry = new QuadArray(4, QuadArray.COORDINATES | QuadArray.TEXTURE_COORDINATE_2);
        kGeometry.setCoordinate(0, new Point3d(-1, -1, -15));
        kGeometry.setCoordinate(1, new Point3d(1, -1, -15));
        kGeometry.setCoordinate(2, new Point3d(1, 1, -15));
        kGeometry.setCoordinate(3, new Point3d(-1, 1, -15));

        Vector4f kCoordMapX = new Vector4f(0.5f, 0.0f, 0.0f, 0.5f);
        Vector4f kCoordMapY = new Vector4f(0.0f, 0.5f, 0.0f, 0.5f);
        TexCoordGeneration kTexCoordGeneration = new TexCoordGeneration(TexCoordGeneration.OBJECT_LINEAR,
                                                                        TexCoordGeneration.TEXTURE_COORDINATE_2,
                                                                        kCoordMapX, kCoordMapY);
        kDefaultAppearance.setTexCoordGeneration(kTexCoordGeneration);

        /* Create the rendered shape, allow the appearance to be read. */
        m_kStereoShape = new Shape3D(kGeometry, kDefaultAppearance);
        m_kStereoShape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        m_kStereoShape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
    }


    /**
     * Read the frame buffer from the input VolumeCanvas3D and store the result in the input BufferedImage:
     *
     * @param  kImage   the image the frame buffer is stored into.
     * @param  kCanvas  the canvas from that is read.
     */
    private void getFrameBuffer(BufferedImage kImage, VolumeCanvas3D kCanvas) {
        int[] pixels;
        int xDim, yDim;
        Robot robot;

        Dimension d = new Dimension();
        Point p = new Point();

        p.x = 0;
        p.y = 0;
        SwingUtilities.convertPointToScreen(p, kCanvas);

        d.width = kCanvas.getWidth();
        d.height = kCanvas.getHeight();

        Rectangle currentRectangle = new Rectangle(p, d);

        /* Using Robot to capture image rectangle and transfering image into
         * the pixel buffer.
         */
        try {
            robot = new Robot();

            Image imagePix = robot.createScreenCapture(currentRectangle);

            xDim = currentRectangle.width;
            yDim = currentRectangle.height;
            xDim = xDim - (xDim % 4);
            yDim = yDim - (yDim % 4);
            pixels = new int[xDim * yDim];

            PixelGrabber pgTest = new PixelGrabber(imagePix, 0, 0, xDim, yDim, pixels, 0, xDim);

            pgTest.grabPixels();
            imagePix = null;
            pgTest = null;
        } catch (InterruptedException e) {
            Preferences.debug("JStereoWindow: Interrupted waiting for pixels!");

            return;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("JStereoWindow: unable to allocate enough memory for RGB image");

            return;
        } catch (AWTException error) {
            MipavUtil.displayError("JStereoWindow: Platform doesn't support screen capture.");

            return;
        }

        int iPixelColor = 0;

        for (int y = 0; y < yDim; y++) {

            for (int x = 0; x < xDim; x++) {
                iPixelColor = 0xffffffff & pixels[(y * xDim) + x];
                iPixelColor = 0xff000000 | iPixelColor;
                kImage.setRGB(x, y, iPixelColor);
            }
        }

        pixels = null;
        robot = null;
    }

    /**
     * Initialize the scene graphs and left, right, and stereo canvases for this JStereoWindow.
     *
     * @param  kNode1  the left-eye object
     * @param  kNode2  the right-eye object
     */
    private void init(BranchGroup kNode1, BranchGroup kNode2) {
        getContentPane().setLayout(new BorderLayout());

        /* Create the left and right image panels to hold the canvases: */
        m_kLeftImagePanel = new JPanel(new BorderLayout());
        m_kRightImagePanel = new JPanel(new BorderLayout());

        /* Create the left canvas with the left object and left image
         * panel: */
        m_kLeftCanvas = createCanvas(kNode1, m_kLeftImagePanel);
        m_kLeftCanvas.setMonoscopicViewPolicy(View.LEFT_EYE_VIEW);

        Point3d kLeftEye = new Point3d();
        m_kLeftCanvas.getView().getPhysicalBody().getLeftEyePosition(kLeftEye);

        /* Create the right canvas with the right object, and right image
         * panel: */
        m_kRightCanvas = createCanvas(kNode2, m_kRightImagePanel);
        m_kRightCanvas.setMonoscopicViewPolicy(View.RIGHT_EYE_VIEW);

        Point3d kRightEye = new Point3d();
        m_kRightCanvas.getView().getPhysicalBody().getRightEyePosition(kRightEye);

        /* Store the initial eye separation: */
        m_dEyeSeparation = kRightEye.x - kLeftEye.x;

        /* Create the texture-mapped polygon and the stereo canvas: */
        createTexturedPolygon();
        m_kStereoPanel = new JPanel(new BorderLayout());
        m_kStereoCanvas = createCanvas(m_kStereoShape, m_kStereoPanel);
        m_kStereoCanvas.getView().setProjectionPolicy(View.PARALLEL_PROJECTION);
    }


    /**
     * Sets up the user interface. "OK" Button and focal-length slider.
     */
    private void setupInterface() {

        /* Eye Separation Slider: */
        m_kEyeSep_Slider = new JSlider(0, 100, (int) (m_dEyeSeparation * 1000));
        m_kEyeSep_Slider.addChangeListener(this);
        m_kEyeSep_Slider.setMajorTickSpacing(10);
        m_kEyeSep_Slider.setPaintTicks(true);
        m_kEyeSep_Slider.setPaintLabels(true);

        /* Close the window: */
        JButton kOK = new JButton("OK");
        kOK.addActionListener(this);
        kOK.setActionCommand("OK_CLOSE");

        /* Update the stereo window: */
        JButton kUpdateStereo = new JButton("Update Stereo");
        kUpdateStereo.addActionListener(this);
        kUpdateStereo.setActionCommand("UpdateStereo");

        /* Slider panel: */
        JPanel kSliderPanel = new JPanel(new GridBagLayout());
        kSliderPanel.add(new JLabel("Eye Separation"));
        kSliderPanel.add(m_kEyeSep_Slider);

        /* Close panel: */
        JPanel kClosePanel = new JPanel(new GridBagLayout());
        kClosePanel.add(kUpdateStereo);
        kClosePanel.add(kOK);

        /* Interface panel holds the above button/slider panels:*/
        JPanel kInterfacePanel = new JPanel();
        GridBagLayout kGBLMat = new GridBagLayout();
        kInterfacePanel.setLayout(kGBLMat);

        GridBagConstraints kgbcMat = new GridBagConstraints();
        kGBLMat.setConstraints(kInterfacePanel, kgbcMat);
        kgbcMat.anchor = GridBagConstraints.CENTER;
        kgbcMat.fill = GridBagConstraints.HORIZONTAL;
        kgbcMat.gridx = 0;
        kgbcMat.gridy = 0;
        kInterfacePanel.add(kSliderPanel, kgbcMat);
        kgbcMat.gridy++;
        kInterfacePanel.add(kClosePanel, kgbcMat);
        kGBLMat.layoutContainer(kInterfacePanel);
        kGBLMat.columnWeights = new double[] { 1 };
        kGBLMat.rowWeights = new double[] { 0, 1, 1, 1, 0 };

        /* kDisplayPane holds the left and right-eye image panels: */
        m_kDisplayPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, m_kLeftImagePanel, m_kRightImagePanel);
        m_kDisplayPane.setDividerSize(6);

        /* kStereoPane holds the m_kDisplayPane and the stereo panel: */
        m_kStereoPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, m_kDisplayPane, m_kStereoPanel);
        m_kStereoPane.setDividerSize(6);

        /* kMainPane holds the m_kStereoPane and the kInterfacePanel */
        m_kMainPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, m_kStereoPane, kInterfacePanel);
        m_kMainPane.setDividerSize(6);
        m_kMainPane.setMinimumSize(new Dimension(1024, 1024));

        /* Add the panels to the window: */
        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(m_kMainPane, BorderLayout.CENTER);
        setSize(new Dimension(1024, 1024));
        setVisible(true);

        /* After the setVisible call, set the divider locations: */
        m_kDisplayPane.setDividerLocation(0.5);
        m_kStereoPane.setDividerLocation(0.5);
        m_kMainPane.setDividerLocation(0.85);
    }


    /**
     * Reads the left and right-eye buffers from the left and right-eye canvases and blends them into the Red-Cyan
     * Anaglyph stereo buffer, which is then texture-mapped on the a polygon and rendered in the stereo canvas.
     */
    private void updateStereo() {

        /* Get the left-eye rendered image from the left canvas: */
        m_kD = m_kLeftCanvas.getSize();

        BufferedImage kLeft = new BufferedImage(m_kD.width, m_kD.height, BufferedImage.TYPE_INT_ARGB);
        getFrameBuffer(kLeft, m_kLeftCanvas);

        /* Get the right-eye rendered image from the right canvas: */
        m_kD = m_kRightCanvas.getSize();

        BufferedImage kRight = new BufferedImage(m_kD.width, m_kD.height, BufferedImage.TYPE_INT_ARGB);
        getFrameBuffer(kRight, m_kRightCanvas);

        /* Blend the two buffers, result stored in the m_kRGStereo buffer:*/
        blendBuffers(kLeft, kRight);

        /* Create the texture and apply it to the m_kStereoShape appearance: */
        ImageComponent2D kDisplayedImage = new ImageComponent2D(ImageComponent.FORMAT_RGBA, m_kRGStereo);

        Texture2D kTexture = new Texture2D(Texture.BASE_LEVEL, Texture.RGBA, m_kD.width, m_kD.height);
        kTexture.setEnable(true);
        kTexture.setMinFilter(Texture.BASE_LEVEL_LINEAR);
        kTexture.setMagFilter(Texture.BASE_LEVEL_LINEAR);
        kTexture.setBoundaryModeS(Texture.CLAMP_TO_EDGE);
        kTexture.setBoundaryModeT(Texture.CLAMP_TO_EDGE);
        kTexture.setImage(0, kDisplayedImage);
        m_kStereoShape.getAppearance().setTexture(kTexture);
    }
}
