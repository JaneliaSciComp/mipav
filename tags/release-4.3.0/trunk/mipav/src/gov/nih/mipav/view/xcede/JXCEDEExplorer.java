package gov.nih.mipav.view.xcede;


import gov.nih.mipav.view.*;

import org.w3c.dom.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * DOCUMENT ME!
 */
public class JXCEDEExplorer extends JFrame implements TreeSelectionListener, ComponentListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 205018093506676689L;

    /** DOCUMENT ME! */
    public static final String defaultTitle = "XCEDE Schema Explorer";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Document document;

    /** DOCUMENT ME! */
    private JEditorPane inforPane;

    /** DOCUMENT ME! */
    private JScrollPane leftScrollPane;

    /** DOCUMENT ME! */
    private JScrollPane rightScrollPane;

    /** DOCUMENT ME! */
    private JSplitPane splitPane;

    /** DOCUMENT ME! */
    private JXCEDETree tree;

    /** DOCUMENT ME! */
    private XCEDEDOMToTreeModelAdapter treeModel;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a <code>JXCEDEExplorer</code> object.
     *
     * @param  document  the XCEDE file(.bxh)
     */
    public JXCEDEExplorer(Document document) {
        this(defaultTitle, document);
    }

    /**
     * Creates a new JXCEDEExplorer object.
     *
     * @param  title     DOCUMENT ME!
     * @param  document  DOCUMENT ME!
     */
    public JXCEDEExplorer(String title, Document document) {
        super(title);

        userInterface = ViewUserInterface.getReference();
        userInterface.getMainFrame().addComponentListener(this);
        this.document = document;
        treeModel = new XCEDEDOMToTreeModelAdapter(document);
        tree = new JXCEDETree(document);
        //        tree.createPopupMenu();
        tree.addTreeSelectionListener(this);
        init();

        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
        }

        /**
         * Tells the swing to dispose this frame when it is closed.
         */
        this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void componentHidden(ComponentEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void componentMoved(ComponentEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void componentResized(ComponentEvent e) {
        Object source = e.getSource();

        if (source instanceof JFrame) {
            JFrame frame = (JFrame) source;
            Dimension size = this.getPreferredSize();
            Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

            GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
            GraphicsDevice[] gs = ge.getScreenDevices();
            int height = (int) frame.getPreferredSize().getHeight();
            Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(gs[0].getDefaultConfiguration());
            this.setSize(new Dimension(300, (int) (screenSize.getHeight() - height - insets.bottom)));
            this.setLocation(frame.getLocation().x, frame.getLocation().y + height);
            splitPane.setDividerLocation((int) (this.getSize().getHeight() * 0.8));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void componentShown(ComponentEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  document  DOCUMENT ME!
     */
    public void setDocument(Document document) {
        this.document = document;
        treeModel = new XCEDEDOMToTreeModelAdapter(document);
        tree.setModel(treeModel);
    }

    /**
     * The <code>TreeSelectionEvent</code> handler.
     *
     * @param  e  DOCUMENT ME!
     */
    public void valueChanged(TreeSelectionEvent e) {
        Object obj = e.getPath().getLastPathComponent();

        if (obj != null) {
            XCEDEAdapterNode adapterNode = (XCEDEAdapterNode) obj;
            inforPane.setText(adapterNode.content());

            if (adapterNode.toString().equals("datarec")) {

                if ((adapterNode.getOpenedImageFrame() != null) &&
                        (adapterNode.getOpenedImageFrame().getImageA() != null)) {
                    adapterNode.getOpenedImageFrame().setVisible(true);
                    adapterNode.getOpenedImageFrame().toFront();
                } else {
                    XCEDEAdapterNode childNode = adapterNode.child("filename").child("#text");
                    ViewUserInterface.getReference().openImageFrame(Preferences.getProperty("ImageDirectory") +
                                                                    childNode.getNodeValue(), true);
                    adapterNode.setOpenedImageFrame(ViewUserInterface.getReference().getActiveImageFrame());
                }
            }
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void windowActivated(WindowEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void windowClosed(WindowEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent e) {
        this.userInterface.setXCEDEExplorer(null);
        this.userInterface.getMainFrame().removeComponentListener(this);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void windowDeactivated(WindowEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void windowDeiconified(WindowEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void windowIconified(WindowEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void windowOpened(WindowEvent e) { }

    /**
     * Cleans up the memory.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        System.out.println("Entering the " + JXCEDEExplorer.class.getName() + " finalize() ... ");
        tree = null;
        userInterface = null;
        leftScrollPane = null;
        rightScrollPane = null;
        splitPane = null;
        inforPane = null;
        treeModel = null;
        document = null;
        super.finalize();
    }

    /**
     * Initializes the user interface of the <code>JXCEDEExplorer</code>.
     */
    private void init() {
        inforPane = new JEditorPane("text/html", "");
        leftScrollPane = new JScrollPane(tree);
        rightScrollPane = new JScrollPane(inforPane);
        splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, leftScrollPane, rightScrollPane);
        splitPane.setOneTouchExpandable(true);

        this.addWindowListener(this);
        this.getContentPane().add(splitPane);

        int x = userInterface.getMainFrame().getX();
        int y = userInterface.getMainFrame().getY();
        int height = userInterface.getMainFrame().getHeight();
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] gs = ge.getScreenDevices();
        Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(gs[0].getDefaultConfiguration());

        // JFrame.setPreferredSize() is not available before jvm 1.5
        // this.setPreferredSize(new Dimension(300, (int)screenSize.getHeight()-height-insets.bottom));
        this.setSize(new Dimension(300, (int) screenSize.getHeight() - height - insets.bottom));
        this.setLocation(x, y + height);

        //        this.setLocation(new Point(0, ));
        pack();
        splitPane.setDividerLocation((int) (this.getSize().getHeight() * 0.80));
        this.setVisible(true);
    }
}
