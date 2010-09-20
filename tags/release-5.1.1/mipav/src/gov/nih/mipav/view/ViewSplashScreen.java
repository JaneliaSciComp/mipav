package gov.nih.mipav.view;


import java.awt.*;
import java.awt.event.*;

import java.net.*;

import javax.swing.*;


/**
 * Shows the MIPAV splash screen until the user clicks the image or a few seconds (4 currently).
 *
 * @see     ViewUserInterface#showSplashGraphics()
 * @author  orsinol
 */
public class ViewSplashScreen extends JFrame implements MouseListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 924235664168779459L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The splash screen image. */
    protected Image image;

    /** Whether the splash screen was loaded from disk successfully. */
    protected boolean loadOK;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Setup and display the splash screen.
     */
    public ViewSplashScreen() {
        super();

        // add listeners that will close this splash screen
        addMouseListener(this);
        addWindowListener(this);

        // do not show title bar
        setUndecorated(true);

        image = readImage("splash.png");

        if (image == null) // if failed, do nothing
        {
            Preferences.debug("Failed to load splash screen.\n", Preferences.DEBUG_MINOR);
            loadOK = false;

            return;
        }

        loadOK = true;

        buildGUI();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Method determines whether image loading failed. Called by UI to determine whether it should call wait().
     *
     * @return  boolean
     */
    public boolean loadOK() {
        return loadOK;
    }

    /**
     * Do nothing.
     *
     * @param  event  mouse event -- ignored
     */
    public void mouseClicked(MouseEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  mouse event -- ignored
     */
    public void mouseEntered(MouseEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  mouse event -- ignored
     */
    public void mouseExited(MouseEvent event) { }

    /**
     * Wakes up the VUI thread on a user's mouse click in the image if it is sleeping while waiting for the splash
     * screen to be displayed.
     *
     * @param  event  mouse event
     */
    public void mousePressed(MouseEvent event) {

        synchronized (ViewUserInterface.getReference()) {
            ViewUserInterface.getReference().notifyAll();
        }
    }

    /**
     * Do nothing.
     *
     * @param  event  mouse event -- ignored
     */
    public void mouseReleased(MouseEvent event) { }

    /**
     * Paint the image into the frame.
     *
     * @param  graphics  used to draw the image and version string in the frame
     */
    public void paint(Graphics graphics) {
        graphics.drawImage(image, 0, 0, null);

        drawVersionString(graphics);
    }

    /**
     * Do nothing.
     *
     * @param  event  window event -- ignored
     */
    public void windowActivated(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  window event -- ignored
     */
    public void windowClosed(WindowEvent event) { }

    /**
     * Wakes up the VUI thread on the user's closing of the splash screen window if it is sleeping while waiting for the
     * splash screen to be displayed.
     *
     * @param  event  window event
     */
    public void windowClosing(WindowEvent event) {

        synchronized (ViewUserInterface.getReference()) {
            ViewUserInterface.getReference().notifyAll();
        }
    }

    /**
     * Do nothing.
     *
     * @param  event  window event -- ignored
     */
    public void windowDeactivated(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  window event -- ignored
     */
    public void windowDeiconified(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  window event -- ignored
     */
    public void windowIconified(WindowEvent event) { }

    /**
     * Do nothing.
     *
     * @param  event  window event -- ignored
     */
    public void windowOpened(WindowEvent event) { }

    /**
     * GUI initialization.
     */
    protected void buildGUI() {
        setSize(image.getWidth(null), image.getHeight(null));

        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

        getContentPane().setLayout(new BorderLayout());

        setResizable(false);

        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        MipavUtil.centerOnScreen(this);
    }

    /**
     * Get the version string and draw it on the image.
     *
     * @param  graphics  Graphics
     */
    protected void drawVersionString(Graphics graphics) {
        Font font = new Font("Helvetica", Font.PLAIN, 9);

        graphics.setFont(font);
        graphics.setColor(Color.black);

        FontMetrics fontMetrics = graphics.getFontMetrics();

        String versionString = "v" + MipavUtil.getVersion();

        int stringWidth = fontMetrics.stringWidth(versionString);

        // calculate width of text in order to right-jusitfy it in the frame
        graphics.drawString(versionString, getSize().width - stringWidth - 2,
                            getSize().height - fontMetrics.getDescent() - 2); // the - 4 is to pad the edge 4 pixels
    }

    /**
     * Read the splash screen image from disk.
     *
     * @param   filename  the image file name relative to MIPAV's working directory
     *
     * @return  the image
     */
    protected Image readImage(String filename) {
        URL imgURL = getClass().getClassLoader().getResource(filename);

        if (imgURL == null) {
            Preferences.debug("Unable to find " + filename +
                              " in the directory or jar packgage where MipavMain.class is located.\n",
                              Preferences.DEBUG_MINOR);

            return null;
        }

        ImageIcon imageIcon = new ImageIcon(imgURL);

        if ((imageIcon.getImageLoadStatus() == MediaTracker.ABORTED) ||
                (imageIcon.getImageLoadStatus() == MediaTracker.ERRORED)) {
            return null;
        }

        return imageIcon.getImage();
    }
}
