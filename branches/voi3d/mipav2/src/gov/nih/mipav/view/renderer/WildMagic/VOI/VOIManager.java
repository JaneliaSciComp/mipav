package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.MipavCoordinateSystems;
import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.PointStack;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOILine;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIPolyLineSlice;
import gov.nih.mipav.model.structures.VOIProtractor;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.CustomUIBuilder;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.RubberbandLivewire;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJPopupPt;
import gov.nih.mipav.view.ViewJPopupVOI;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.dialogs.JDialogAnnotation;
import gov.nih.mipav.view.dialogs.JDialogVOISplitter;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.BitSet;
import java.util.Stack;
import java.util.Vector;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;

import com.mentorgen.tools.profile.runtime.Profile;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

public class VOIManager implements ActionListener, KeyListener, MouseListener, MouseMotionListener
{
    /**
     * Binary search tree, modified so it will work with elements that have the same cost. In theory this should not be
     * a problem because we have a unique identifier in the parameter <code>position</code>. New nodes of the same cost
     * as an existing node are inserted to the left of the first existing node of the same cost. The children of the
     * first existing node are transferred to the left of the new node. The new node is not put all the way to the left
     * of a chain of nodes of the same cost so as to save tree traversal time. This convention must be followed in the
     * remove and pop or else some nodes get lost and others are never removed.
     *
     * <p>The main reason for using a Binary Search Tree is that the cost of a "find" (required for remove, pop, and
     * insert) is log n if the tree is reasonably balanced. To insure balance we could have used a red-black tree, but
     * test runs indicate that this tree stays fairly balanced on its own due to the nature of the insertions and
     * deletions.</p>
     */
    public class ActiveTree {

        /** Pointer to root of tree. */
        TreeNode root;

        /**
         * Sets root node to null.
         */
        public ActiveTree() {
            root = null;
        }

        /**
         * Prints out tree in sorted order. Level allows user to see the structure of the tree.
         */
        public void inOrder() {
            inOrder(root, 0);
        }

        /**
         * Inserts node with given position and cost at the appropriate location in the tree. If the cost is the same as
         * one previously in the tree, inserts the node on the left of the original.
         *
         * @param  position  Position in image array.
         * @param  cost      Cost at this position.
         */
        public void insert(int position, float cost) {

            if (root == null) { // set root to a new node
                root = new TreeNode(position, cost);
            } else { // find appropriate parent of node we wish to insert

                TreeNode node = root;
                TreeNode parent = null;

                // leaf node, add there   this should never happen
                while ((node != null) && (node.position != position)) {
                    parent = node;

                    if (node.cost > cost) {
                        node = node.left;
                    } else if (node.cost < cost) {
                        node = node.right;
                    } else { // inserting a node of the same cost, special case
                        node = node.left;

                        break;
                    }
                }

                if (parent.cost > cost) {
                    parent.left = new TreeNode(position, cost);
                } else if (parent.cost < cost) {
                    parent.right = new TreeNode(position, cost);
                } else { // inserting node of same cost
                    parent.left = new TreeNode(position, cost);

                    // if old node had children, make them child of new node
                    if (node != null) {
                        parent.left.left = node;
                    }
                }
            }
        }

        /**
         * Returns a flag indicating if this tree is empty.
         *
         * @return  <code>true</code> if tree is empty.
         */
        public final boolean isEmpty() {
            return root == null;
        }

        /**
         * Pops off minimum cost in tree. Returns position of minimum cost and also removes that node from the tree.
         *
         * @return  Position of minimum cost.
         */
        public final int pop() {
            TreeNode node = root;
            TreeNode parent = null;

            // find leftmost node, this is the minimum
            while (node.left != null) {
                parent = node;
                node = node.left;
            }

            // if both node.left and node.right are null, leaf node.
            if (node.right == null) {

                if (parent != null) {
                    parent.left = null;
                } else {
                    root = null;
                }
            } else { // not a leaf node, there is still a right node attached.

                if (parent != null) {
                    parent.left = node.right;
                } else {
                    root = node.right;
                }
            }

            return node.position;
        }

        /**
         * Removes node of given position and cost. <code>cost</code> is used to find where the node is; <code>
         * position</code> is used to uniquely identify the node.
         *
         * @param  position  Position in image array of node to remove.
         * @param  cost      Cost of function at position.
         */
        public void remove(int position, float cost) {
            TreeNode node = root;
            TreeNode parent = null;

            // Find node and parent node
            while ((node != null) && (node.position != position)) {
                parent = node;

                if (node.cost > cost) {
                    node = node.left;
                } else if (node.cost < cost) {
                    node = node.right;
                } else {
                    node = node.left;
                }
            }

            if (node == null) {
                Preferences.debug("Tried to remove " + position + " " + cost + " which does not exist.\n");
            }

            if ((node.right == null) && (node.left == null)) { // Leaf node

                if (node != root) {

                    if (parent.cost >= cost) {
                        parent.left = null;
                    } else {
                        parent.right = null;
                    }
                } else {
                    root = null;
                }
            } else if ((node.right == null) && (node.left != null)) { // right subtree empty, left subtree not.

                if (node != root) {

                    if (parent.cost >= cost) {
                        parent.left = node.left;
                    } else {
                        parent.right = node.left;
                    }
                } else {
                    root = node.left;
                }
            } else if ((node.right != null) && (node.left == null)) { // left subtree empty, right subtree not.

                if (node != root) {

                    if (parent.cost >= cost) {
                        parent.left = node.right;
                    } else {
                        parent.right = node.right;
                    }
                } else {
                    root = node.right;
                }
            } else { // left and right subtrees not empty

                TreeNode rightmost = node.left;
                TreeNode rightmostP = node;

                while (rightmost.right != null) {
                    rightmostP = rightmost;
                    rightmost = rightmost.right;
                }

                node.position = rightmost.position;
                node.cost = rightmost.cost;

                if (rightmostP != node) {
                    rightmostP.right = rightmost.left;
                } else {
                    node.left = rightmost.left;
                }
            }
        }

        /**
         * Sets root to null.
         */
        public void reset() {
            root = null;
        }

        /**
         * Prints out tree in sorted order. Level allows user to see the structure of the tree. Will print out tree from
         * given node on down.
         *
         * @param  node   Root of tree to print.
         * @param  level  Level of tree to print.
         */
        private void inOrder(TreeNode node, int level) {

            if (node != null) {
                inOrder(node.left, level + 1);
                System.out.println("Node " + node + " at level " + level);
                inOrder(node.right, level + 1);
            }
        }
    }
    /**
     * Tree node. Contains data (the integer position and float cost) and pointers to the left and right children.
     */
    public class TreeNode {

        /** Cost of position. */
        public float cost;

        /** Node to the left in the tree. */
        public TreeNode left;

        /** Position in array seededCosts. */
        public int position;

        /** Node to the right in the tree. */
        public TreeNode right;

        /**
         * Creates a new tree node with the given data.
         *
         * @param  position  Position in array seededCosts.
         * @param  cost      Cost of node.
         */
        public TreeNode(int position, float cost) {
            right = null;
            left = null;
            this.position = position;
            this.cost = cost;
        }

        /**
         * Returns readable representation of this node.
         *
         * @return  Readable representation of node.
         */
        public String toString() {
            return "position: " + position + " cost: " + cost;
        }
    }
    private boolean m_bDrawVOI = false;

    private boolean m_bPointer = false;
    private boolean m_bSelected = false;


    /** used in the popup menu when the user right-clicks over a voi intensity line. */
    public static final String DELETE_INTENSITY_LINE = "delete_inensity_line";
    public static final String SHOW_INTENSITY_GRAPH = "show_intensity_graph";

    private static final int TEXT = 0;
    private static final int POINT = 1;
    public static final int POLYPOINT = 2;
    private static final int LINE = 3;
    private static final int PROTRACTOR = 4;
    private static final int RECTANGLE = 5;
    private static final int OVAL = 6;
    private static final int POLYLINE = 7;
    private static final int LEVELSET = 8;
    private static final int LIVEWIRE = 9;
    private static final int RECTANGLE3D = 10;
    private static final int SPLITLINE = 11;
    private static final int LUT = 12;
    private int m_iDrawType;

    private VOIBase m_kCurrentVOI = null;
    private VOIBase m_kCopyVOI = null;
    protected ScreenCoordinateListener m_kDrawingContext = null;
    private PointStack levelSetStack = new PointStack(500);
    private BitSet map = null;
    private Stack<int[]> stack = new Stack<int[]>();
    private int m_iSlice;
    private boolean m_bLeftMousePressed;

    /** Change the mouse cursor with the first mouseDrag event */
    private boolean m_bFirstDrag = true;

    private float m_fMouseX, m_fMouseY;
    private int m_iPlaneOrientation;
    private ModelImage[] m_akImages = new ModelImage[2];
    private ModelImage m_kImageActive;
    private ModelImage m_kLocalImage;

    private int[] m_aiLocalImageExtents;

    private VOIManagerInterface m_kParent;

    private boolean m_bFirstVOI = true;
    private int m_iCirclePts = 32;

    private double[] m_adCos = new double[m_iCirclePts];
    private double[] m_adSin = new double[m_iCirclePts];
    private static final int NearNone = -1;
    private static final int NearPoint = 0;

    private static final int NearLine = 1;

    private int m_iNearStatus = NearNone;

    private Component m_kComponent = null;

    private int m_iLiveWireSelection = 0;

    Vector3f m_kCenter = new Vector3f();

    private ActiveTree activeTree;

    private byte[] costGraph = null;



    private float grad_weight = 0.20f; // used to remember gradient weight


    private float[] localCosts = null;

    private BitSet processedIndicies;
    private float[] seededCosts;

    private int seedPoint;

    private boolean[] m_abInitLiveWire;

    private boolean m_bLiveWireInit = false;    

    private float[] xDirections;


    private float[] yDirections;

    private float[] imageBufferActive;

    private boolean m_bQuickLUT = false;




    /** Popup Menu for VOIs (non-point). */
    protected ViewJPopupVOI m_kPopupVOI = null;

    /** Popup Menu for VOIPoints. */
    protected ViewJPopupPt m_kPopupPt = null;

    private int m_iPlane = -1;


    public VOIManager (VOIManagerInterface kParent )
    {
        m_kParent = kParent;
    }

    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals(DELETE_INTENSITY_LINE)) // handling the popup menu for the VOI intensity line
        {
            m_kParent.deleteVOI( m_kCurrentVOI );
            m_kParent.setDefaultCursor();
        }
        else if (command.equals(SHOW_INTENSITY_GRAPH)) // handling the popup menu for the VOI intensity line
        {
            m_kParent.showIntensityGraph( m_kCurrentVOI );
        }
    }


    public void add( VOIBase kVOI, float fHue )
    {
        m_kParent.newVOI( false, false );
        m_kParent.addVOI( kVOI, true );
        m_kParent.setPresetHue(fHue);
        kVOI.setActive(true);
        m_kParent.setDefaultCursor();
    }

    public boolean add( VOIBase kVOI, int iPos, Vector3f kNewPoint, boolean bIsFile  )
    {
        if ( kVOI.isFixed() )
        {
            return false;
        }        
        Vector3f kFilePt = kNewPoint;
        if ( !bIsFile )
        {            
            kFilePt = new Vector3f();
            m_kDrawingContext.screenToFile( (int)kNewPoint.X, (int)kNewPoint.Y, (int)kNewPoint.Z, kFilePt);
        }
        if ( (iPos + 1) < kVOI.size() )
        {
            kVOI.insertElementAt( kFilePt, iPos + 1);
        }
        else
        {
            if ( (kVOI.size() == 0) || !kVOI.lastElement().equals( kFilePt ) )
            {
                kVOI.add( kFilePt );
            }
            else
            {
                return false;
            }
        }
        kVOI.setSelectedPoint( iPos + 1 );     
        kVOI.update();

        return true;
    }

    public boolean add( VOIBase kVOI, Vector3f kNewPoint, boolean bIsFile  )
    {
        return add( kVOI, kVOI.size() - 1, kNewPoint, bIsFile );
    }

    public void anchor(int iX, int iY, boolean bSeed) {
        Vector3f kNewPoint = new Vector3f( iX, iY, m_iSlice ) ;
        if ( m_kCurrentVOI == null )
        {
            Vector<Vector3f> kPositions = new Vector<Vector3f>();
            kPositions.add( kNewPoint );
            m_kCurrentVOI = createVOI( m_iDrawType, false, false, kPositions );
            m_kParent.addVOI( m_kCurrentVOI, true );
        }
        else
        {
            if ( add( m_kCurrentVOI, kNewPoint, false ) )
            {
                m_kParent.updateDisplay();
            }
        }
        m_kCurrentVOI.setAnchor();
        if ( bSeed )
        {
            seed(iX, iY);
        }
    }

    public void anchorPolyline(int iX, int iY, boolean bFinished) {
        Vector3f kNewPoint = new Vector3f( iX, iY, m_iSlice ) ;
        if ( m_kCurrentVOI == null )
        {
            Vector<Vector3f> kPositions = new Vector<Vector3f>();
            kPositions.add( kNewPoint );
            m_kCurrentVOI = createVOI( m_iDrawType, false, false, kPositions );
            m_kParent.addVOI( m_kCurrentVOI, true );
        }
        else
        {
            m_kCurrentVOI.setSize( m_kCurrentVOI.getAnchor()+1 );
            if ( add( m_kCurrentVOI, kNewPoint, false ) )
            {
                m_kParent.updateDisplay();
            }
        }
        m_kCurrentVOI.setAnchor();
        if ( !bFinished )
        {
            m_kCurrentVOI.setActive(true);
        }
        else
        {
            //m_kCurrentVOI.setSize( m_kCurrentVOI.getAnchor()-1 );
        }
    }


    public boolean contains( VOIBase kVOI, int iX, int iY, int iZ ) {

        Vector3f kVolumePt = new Vector3f();
        m_kDrawingContext.screenToFile( iX, iY, m_iSlice, kVolumePt );
        if ( kVOI.contains( kVolumePt.X, kVolumePt.Y, kVolumePt.Z ) )
        {
            return true;
        }
        if ( kVOI.nearLine( (int)kVolumePt.X, (int)kVolumePt.Y, (int)kVolumePt.Z ) )
        {
            return true;
        }
        return kVOI.nearPoint( iX, iY, iZ );
    }


    public void deleteVOI( VOIBase kVOI )
    {
        m_kParent.deleteVOI( kVOI );
    }
    public int deleteVOIActivePt( VOIBase kVOI )
    {
        int iPos = kVOI.getSelectedPoint();
        if ( iPos < 0 )
        {
            return kVOI.size();
        }
        kVOI.delete( kVOI.getSelectedPoint() );      
        if ( (kVOI.size() == 0) && (kVOI == m_kCurrentVOI) )
        {
            m_kCurrentVOI = null;
        }
        return kVOI.size();
    }

    public void dispose() 
    {
        m_kCurrentVOI = null;
        m_kCopyVOI = null;
        m_kDrawingContext = null;
        levelSetStack = null;
        map = null;
        stack = null;
        m_akImages[0] = null;
        m_akImages[1] = null;
        m_akImages = null;
        m_kImageActive = null;
        if ( m_kLocalImage != null )
        {
            m_kLocalImage.disposeLocal();
            m_kLocalImage = null;
        }

        m_aiLocalImageExtents = null;
        m_kParent = null;

        m_adCos = null;
        m_adSin = null;

        m_kComponent = null;

        m_kCenter = null;

        activeTree = null;
        costGraph = null;
        localCosts = null;
        processedIndicies = null;
        seededCosts = null;
        m_abInitLiveWire = null;
        xDirections = null;
        yDirections = null;
    }

    public void doVOI( String kCommand, boolean isDrawCommand )
    {
        if ( m_bFirstVOI )
        {
            m_bFirstVOI = false;            
            for ( int i = 0; i < m_iCirclePts; i++ )
            {
                m_adCos[i] = Math.cos( Math.PI * 2.0 * i/m_iCirclePts );
                m_adSin[i] = Math.sin( Math.PI * 2.0 * i/m_iCirclePts);
            }
        }
        m_bDrawVOI = false;
        m_bPointer = false;


        if (kCommand.equals(CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER.getActionCommand()) ) {
            m_bPointer = true;
        }
        else if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_NEW.getActionCommand()) || 
                kCommand.equals("ResetVOI") )
        {
            m_kCurrentVOI = null;
        }
        else if ( isDrawCommand )
        {
            m_bDrawVOI = true;
            m_bSelected = false;
            if ( m_kCurrentVOI != null && !m_bQuickLUT )
            {
                if ( !kCommand.equals(CustomUIBuilder.PARAM_VOI_SPLITTER.getActionCommand()) )
                {
                    m_kCurrentVOI.setActive(false);
                }
                m_kCurrentVOI = null;
            }

            if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_PROTRACTOR.getActionCommand()) )
            {
                m_iDrawType = PROTRACTOR;
            }
            else if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_LIVEWIRE.getActionCommand()) )
            {
                m_iDrawType = LIVEWIRE;
            }
            else if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_SPLITTER.getActionCommand()) )
            {
                m_iDrawType = SPLITLINE;
            }
            else if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_LINE.getActionCommand()) )
            {
                m_iDrawType = LINE;
            }
            else if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_POLY_SLICE.getActionCommand()) )
            {
                m_iDrawType = POLYPOINT;
            }
            else if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_POINT.getActionCommand()) ) {
                m_iDrawType = POINT;
            }
            else if ( kCommand.equals(CustomUIBuilder.PARAM_VOI_TEXT.getActionCommand()) ) {
                m_iDrawType = TEXT;
            }
            else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_3D_RECTANGLE.getActionCommand()) ) {
                m_iDrawType = RECTANGLE3D;
            } 
            else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_RECTANGLE.getActionCommand()) ) {
                m_iDrawType = RECTANGLE;
            } 
            else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_ELLIPSE.getActionCommand()) ) {
                m_iDrawType = OVAL;
            } 
            else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_POLYGON.getActionCommand()) ) {
                m_iDrawType = POLYLINE;
            } 
            else if (kCommand.equals(CustomUIBuilder.PARAM_VOI_LEVELSET.getActionCommand()) ) {
                m_iDrawType = LEVELSET;
            } 
            else if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_QUICK.getActionCommand()) ) {
                m_iDrawType = LUT;
            }
        }
    }

    public void draw( VOIBase kVOI, float[] resolutions, int[] unitsOfMeasure, 
            int slice, int orientation, Graphics g ) 
    {
        if (g == null) {
            MipavUtil.displayError("Draw VOI: grapics = null");
            return;
        }
        if ( kVOI.getType() == VOI.POLYLINE_SLICE )
        {
            drawVOIPolyLineSlice( (VOIPolyLineSlice)kVOI, resolutions, unitsOfMeasure, g, slice, orientation );
        }
        if ( (m_iPlane != (m_iPlane & kVOI.getPlane())) || (getSlice(kVOI)!= slice) )
        {
            return;
        }
        switch ( kVOI.getType() )
        {
        case VOI.CONTOUR:
        case VOI.POLYLINE:
            drawVOI( kVOI, resolutions, unitsOfMeasure, g, 1 );
            return;

        case VOI.LINE:
            drawVOILine( kVOI, resolutions, unitsOfMeasure, g, 1 );
            return;
        case VOI.POINT:
            drawVOIPoint( kVOI, resolutions, unitsOfMeasure, g );
            return;
        case VOI.POLYLINE_SLICE:
            return;
        case VOI.ANNOTATION:
            drawVOIText( ((VOIText)kVOI), g );
            return;
        case VOI.PROTRACTOR:
            drawVOIProtractor( kVOI, resolutions, unitsOfMeasure, g );
            return;
        }

    }

    public void drawNext(int iX, int iY) {
        if ( m_kCurrentVOI == null )
        {
            return;
        }
        while ( m_kCurrentVOI.getAnchor() < (m_kCurrentVOI.size()-1) )
        {
            m_kCurrentVOI.delete(m_kCurrentVOI.getAnchor()+1);
        }

        float fY = iY;
        Vector3f kNewPoint = new Vector3f( iX, fY, m_iSlice ) ;
        add( m_kCurrentVOI, kNewPoint, false );

        Vector3f kFile = new Vector3f();
        m_kDrawingContext.screenToFile(iX, (int)fY, m_iSlice, kFile);
        Vector3f kLocalPt = fileCoordinatesToPatient(kFile);
        int xDim = m_aiLocalImageExtents[0];
        int x = (int)kLocalPt.X;
        int y = (int)kLocalPt.Y;

        int mouseIndex = (y * xDim) + x;

        int liveWirePtIndex = mouseIndex + convertGraphToInt(costGraph[mouseIndex], xDim);
        if (liveWirePtIndex == seedPoint) {
            return;
        }

        boolean bPointsAdded = false;
        int iAnchor = m_kCurrentVOI.getAnchor();
        while (liveWirePtIndex != seedPoint) {
            // add mouseIndex point:
            x = mouseIndex % xDim;
            y = (mouseIndex - x)/xDim;
            kNewPoint = new Vector3f( x, y, m_iSlice ) ;
            Vector3f kVolumePt = patientCoordinatesToFile(kNewPoint);
            if ( add( m_kCurrentVOI, iAnchor, kVolumePt, true ) )
            {
                bPointsAdded = true;
            }

            // System.out.println(" Draw next x: = " + xPoints[count] + " y = " + yPoints[count] + "  seed = " +
            // seedPoint + " location = " +  location);
            mouseIndex = liveWirePtIndex;
            liveWirePtIndex = mouseIndex + convertGraphToInt(costGraph[mouseIndex], xDim);
        }

        // add mouseIndex point:
        x = mouseIndex % xDim;
        y = (mouseIndex - x)/xDim;
        kNewPoint = new Vector3f( x, y, m_iSlice ) ;
        Vector3f kVolumePt = patientCoordinatesToFile(kNewPoint);
        if ( add( m_kCurrentVOI, iAnchor, kVolumePt, true ) )
        {
            bPointsAdded = true;
        }
        if ( bPointsAdded )
        {
            m_kParent.updateDisplay();
        }
    }

    public void drawNextPolyline(int iX, int iY) {
        if ( m_kCurrentVOI == null )
        {
            return;
        }

        m_kCurrentVOI.setSize( m_kCurrentVOI.getAnchor()+1 );

        float fY = iY;
        Vector3f kNewPoint = new Vector3f( iX, fY, m_iSlice ) ;
        if ( add( m_kCurrentVOI, kNewPoint, false ) )
        {
            m_kParent.updateDisplay();
        }
    }

    public Vector3f fileCoordinatesToPatient( Vector3f volumePt )
    {       
        Vector3f kPatient = new Vector3f();
        MipavCoordinateSystems.fileToPatient( volumePt, kPatient, m_kImageActive, m_iPlaneOrientation );
        return kPatient;
    }



    public ModelImage getActiveImage()
    {
        return m_kImageActive;
    }

    public Component getComponent()
    {
        return m_kComponent;
    }


    public VOIBase getCurrentVOI()
    {
        return m_kCurrentVOI;
    }

    public int getOrientation()
    {
        return m_iPlaneOrientation;
    }

    public VOIManagerInterface getParent()
    {
        return m_kParent;
    }

    public int getPlane()
    {
        return m_iPlane;
    }

    public void init( ModelImage kImageA, ModelImage kImageB, Component kComponent, 
            ScreenCoordinateListener kContext, int iOrientation, int iSlice )
    {
        m_akImages[0] = kImageA;
        m_akImages[1] = kImageB;
        if ( kImageA != null )
        {
            m_kImageActive = kImageA;
        }
        else
        {
            m_kImageActive = kImageB;
        }
        setCanvas(kComponent);
        setDrawingContext(kContext);
        setOrientation(iOrientation);
        setSlice(iSlice);
        int iPlane = MipavCoordinateSystems.getAxisOrder( kImageA, iOrientation)[2];
        switch ( iPlane )
        {
        case 0: m_iPlane = VOIBase.XPLANE; break;
        case 1: m_iPlane = VOIBase.YPLANE; break;
        case 2: m_iPlane = VOIBase.ZPLANE; break;
        }
    }

    public boolean isActive()
    {
        return (m_bDrawVOI || m_bPointer);
    }


    public void keyPressed(KeyEvent e) {
        //System.err.println("VOIManager keyPressed" );
        if ( e.getKeyChar() == 'q' || e.getKeyChar() == 'Q' )
        {
            if ( !m_bQuickLUT && m_kCurrentVOI != null )
            {
                m_kCurrentVOI.setActive(false);
                m_kCurrentVOI = null;
            }
            m_bQuickLUT = true;
        }
        final int keyCode = e.getKeyCode();        
        switch (keyCode) {
        case KeyEvent.VK_UP:     m_kParent.doVOI("MoveUP"); return;
        case KeyEvent.VK_DOWN:   m_kParent.doVOI("MoveDown"); return;
        case KeyEvent.VK_LEFT:   m_kParent.doVOI("MoveLeft"); return;
        case KeyEvent.VK_RIGHT:  m_kParent.doVOI("MoveRight"); return;
        }
        KeyStroke ks = KeyStroke.getKeyStrokeForEvent(e);
        String command = Preferences.getShortcutCommand(ks);
        if (command != null) {
            m_kParent.doVOI( command );
        }
    }




    public void keyReleased(KeyEvent e) {
        //System.err.println("VOIManager keyReleased" );
        if ( e.getKeyChar() == 'q' || e.getKeyChar() == 'Q' )
        {
            m_bQuickLUT = false;
            if ( m_kCurrentVOI != null )
            {
                m_bLeftMousePressed = false;
                m_bFirstDrag = true;
                m_kParent.quickLUT(m_kCurrentVOI);
                m_bDrawVOI = false;
            }
        }
        final int keyCode = e.getKeyCode();
        switch (keyCode) {
        case KeyEvent.VK_DELETE:
            if (e.isShiftDown()) 
            {
                m_kParent.doVOI("deleteVOIActivePt");
            } 
            else 
            {
                m_kParent.doVOI(CustomUIBuilder.PARAM_VOI_POINT_DELETE.getActionCommand());
            }
            return;
        }
    }

    public void keyTyped(KeyEvent e)
    {
        //System.err.println("VOIManager keyTyped" );
        if ( e.getKeyChar() == 'q' || e.getKeyChar() == 'Q' )
        {
            if ( !m_bQuickLUT && m_kCurrentVOI != null )
            {
                m_kCurrentVOI.setActive(false);
                m_kCurrentVOI = null;
            }
            m_bQuickLUT = true;
        }
    }

    public void liveWire( int iSelection )
    {
        m_iLiveWireSelection = iSelection;
        if ( m_bFirstVOI )
        {
            m_bFirstVOI = false;            
            for ( int i = 0; i < m_iCirclePts; i++ )
            {
                m_adCos[i] = Math.cos( Math.PI * 2.0 * i/m_iCirclePts );
                m_adSin[i] = Math.sin( Math.PI * 2.0 * i/m_iCirclePts);
            }
        }
        if ( m_kCurrentVOI != null )
        {
            m_kCurrentVOI.setActive(false);
            m_kCurrentVOI = null;
        }
        m_bDrawVOI = true;
        m_bPointer = false;
        m_bSelected = false;
        m_iDrawType = LIVEWIRE;
        initLiveWire( m_iSlice, true );
    }

    public void mouseClicked(MouseEvent kEvent) {
        m_fMouseX = kEvent.getX();
        m_fMouseY = kEvent.getY();
        if ( !isActive() )
        {
            return;
        }
        m_kParent.setActive(this);
        if ( m_bDrawVOI && (m_iDrawType == LIVEWIRE || m_iDrawType == POLYLINE) && (kEvent.getClickCount() > 1) )
        {
            showSelectedVOI( kEvent.getX(), kEvent.getY() );
            if ( m_iNearStatus == NearPoint )
            {
                if ( m_kCurrentVOI.getNearPoint() == 0 )
                {
                    m_kCurrentVOI.setClosed(true);
                }
                else
                {
                    m_kCurrentVOI.setClosed(false);
                }
                if ( m_kCurrentVOI.getGroup() != null )
                {
                    if ( m_kCurrentVOI.getGroup().getCurveType() != m_kCurrentVOI.getType() )
                    {
                        VOI kGroup = m_kCurrentVOI.getGroup();
                        kGroup.getCurves().remove(m_kCurrentVOI);
                        m_kCurrentVOI.setGroup(null);
                        m_kParent.addVOI(m_kCurrentVOI, true);
                    }
                }
            }
            if ( m_iDrawType == LIVEWIRE )
            {
                anchor( kEvent.getX(), kEvent.getY(), false );
            }
            else
            {
                anchorPolyline( kEvent.getX(), kEvent.getY(), true );                
            }
            m_kCurrentVOI.trimPoints(Preferences.getTrim(),
                    Preferences.getTrimAdjacient());
            m_bDrawVOI = false;
            m_iNearStatus = NearNone;
            m_kParent.setDefaultCursor();
        } 
    }

    /* (non-Javadoc)
     * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mouseDragged(java.awt.event.MouseEvent)
     */
    public void mouseDragged(MouseEvent kEvent) {
        if ( !isActive() )
        {
            return;
        }
        m_kParent.setActive(this);
        if (m_bLeftMousePressed) {
            processLeftMouseDrag(kEvent);
        }
    }

    public void mouseEntered(MouseEvent arg0) {
        if ( isActive() )
        {
            //m_kParent.setActive(this);
        }        
    }



    public void mouseExited(MouseEvent arg0) 
    {
        if ( m_bDrawVOI && (m_iDrawType == LEVELSET) )
        {
            if ( m_kCurrentVOI != null )
            {
                m_kParent.deleteVOI(m_kCurrentVOI);
            }
        } 
    }

    public void mouseMoved(MouseEvent kEvent) 
    {      
        if ( !isActive() )
        {
            return;
        }
        m_kParent.setActive(this);
        if ( m_bPointer )
        {
            showSelectedVOI( kEvent.getX(), kEvent.getY() );
        }
        else if ( m_bDrawVOI && (m_iDrawType == LEVELSET) )
        {
            createVOI( kEvent.getX(), kEvent.getY() );
        } 
        else if ( (m_kCurrentVOI != null) && m_bDrawVOI && (m_iDrawType == LIVEWIRE) )
        {
            drawNext( kEvent.getX(), kEvent.getY() );
        } 
        else if ( (m_kCurrentVOI != null) && m_bDrawVOI && (m_iDrawType == POLYLINE) )
        {
            drawNextPolyline( kEvent.getX(), kEvent.getY() );
        } 
    }


    /* (non-Javadoc)
     * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mousePressed(java.awt.event.MouseEvent)
     */
    public void mousePressed(MouseEvent kEvent) {
        m_fMouseX = kEvent.getX();
        m_fMouseY = kEvent.getY();
        if ( !isActive() )
        {
            return;
        }
        m_kParent.setActive(this);

        if (kEvent.getButton() == MouseEvent.BUTTON1) {
            if ( m_bPointer )
            {    
                if ( m_iNearStatus == NearPoint )
                {
                    moveVOIPoint( kEvent.getX(), kEvent.getY() );
                }
                else if ( m_iNearStatus == NearLine )
                {
                    m_kParent.saveVOIs("addVOIPoint");
                    addVOIPoint( kEvent.getX(), kEvent.getY() );
                }
                else
                {
                    selectVOI( kEvent.getX(), kEvent.getY(), kEvent.isShiftDown() );
                }
            }

            m_bLeftMousePressed = true;
            processLeftMouseDrag( kEvent );
        }
        else if ( kEvent.getButton() == MouseEvent.BUTTON3 )
        {
            if ( selectVOI( kEvent.getX(), kEvent.getY(), kEvent.isShiftDown() ) != null )
            {
                if ( m_kCurrentVOI.getType() == VOI.LINE )
                {
                    handleIntensityLineBtn3(kEvent);
                }
                else
                {
                    addPopup( m_kCurrentVOI );
                }
            }
        }
    }

    /* (non-Javadoc)
     * @see WildMagic.LibApplications.OpenGLApplication.JavaApplication3D#mouseReleased(java.awt.event.MouseEvent)
     */
    public void mouseReleased(MouseEvent kEvent) {
        if ( !isActive() )
        {
            return;
        }
        m_kParent.setActive(this);

        if (kEvent.getButton() == MouseEvent.BUTTON1) {
            processLeftMouseDrag( kEvent );
            m_bLeftMousePressed = false;
            m_bFirstDrag = true;
        }
        if ( m_bDrawVOI && ((m_iDrawType == POINT) || (m_iDrawType == POLYPOINT)) )
        {
            createVOI( kEvent.getX(), kEvent.getY() );
        } 
        else if ( m_bDrawVOI && (m_iDrawType == TEXT) )
        {
            createTextVOI( kEvent.getX(), kEvent.getY() );
        } 
        else if ( m_bDrawVOI && (m_iDrawType == RECTANGLE3D) )
        {
            m_kCurrentVOI.setActive(true);
            m_kParent.doVOI(CustomUIBuilder.PARAM_VOI_PROPAGATE_ALL.getActionCommand());
            m_bDrawVOI = false;
            return;
        }
        else if ( m_bDrawVOI && (m_iDrawType == LUT) )
        {
            m_kParent.quickLUT(m_kCurrentVOI);
            m_bDrawVOI = false;
            return;
        }
        else if ( m_bDrawVOI && (m_iDrawType == SPLITLINE) )
        {
            JDialogVOISplitter kSplitDialog = new JDialogVOISplitter(m_kImageActive) ;
            if ( !kSplitDialog.isCancelled()) {
                splitVOIs( kSplitDialog.getAllSlices(), kSplitDialog.getOnlyActive(), m_kCurrentVOI );
            }
            kSplitDialog = null;
            m_bDrawVOI = false;
            m_kParent.setDefaultCursor( );
            return;
        }
        else if ( m_bDrawVOI && ((m_iDrawType == POLYLINE) || (m_iDrawType == LIVEWIRE)) )
        {   
            if ( m_kCurrentVOI != null && m_kCurrentVOI.size() > 2 )
            {
                showSelectedVOI( kEvent.getX(), kEvent.getY() );
                if ( (m_iNearStatus == NearPoint) &&
                        ((m_kCurrentVOI.getNearPoint() == 0) || (m_kCurrentVOI.getNearPoint() == m_kCurrentVOI.size()-2)) )
                {
                    m_kCurrentVOI.setClosed((m_kCurrentVOI.getNearPoint() == 0));
                    m_kCurrentVOI.removeElementAt( m_kCurrentVOI.size() -1 );
                    m_kCurrentVOI.trimPoints(Preferences.getTrim(),
                            Preferences.getTrimAdjacient());
                    if ( m_kCurrentVOI.getGroup() != null )
                    {
                        if ( m_kCurrentVOI.getGroup().getCurveType() != m_kCurrentVOI.getType() )
                        {
                            VOI kGroup = m_kCurrentVOI.getGroup();
                            kGroup.getCurves().remove(m_kCurrentVOI);
                            m_kCurrentVOI.setGroup(null);
                            m_kParent.addVOI(m_kCurrentVOI, true);
                        }
                    }
                    m_bDrawVOI = false;
                    m_iNearStatus = NearNone;
                    m_kParent.setDefaultCursor();
                    return;
                }
            }
            if ( m_iDrawType == LIVEWIRE )
            {
                anchor( kEvent.getX(), kEvent.getY(), true );
            }
            else
            {
                anchorPolyline( kEvent.getX(), kEvent.getY(), false );
            }
            return;
        }


        if ( !kEvent.isShiftDown() && !m_bPointer )
        {
            m_kParent.setDefaultCursor( );
            m_kParent.setSelectedVOI( m_kCurrentVOI.getGroup(), kEvent.isShiftDown() );
        }
        m_iNearStatus = NearNone;
        if ( !kEvent.isShiftDown() || (m_iDrawType == TEXT) 
                || (m_iDrawType == LINE) || (m_iDrawType == PROTRACTOR)
                || (m_iDrawType == POLYLINE) || (m_iDrawType == LIVEWIRE)  
                || (m_iDrawType == RECTANGLE3D) || (m_iDrawType == SPLITLINE) )
        {
            m_bDrawVOI = false;
        }
        else
        {
            m_kCurrentVOI = null;
        }
    }

    public boolean testMove( Vector3f kDiff, Vector3f[] akMinMax )
    {            
        Vector3f kScreenMin = m_kDrawingContext.fileToScreen( akMinMax[0] );
        kScreenMin.Add(kDiff);

        Vector3f kScreenMax = m_kDrawingContext.fileToScreen( akMinMax[1] );
        kScreenMax.Add(kDiff);
        Vector3f kTemp = new Vector3f( kScreenMin );
        kScreenMin.Min( kScreenMax );
        kScreenMax.Max( kTemp );
        if ( m_fMouseX < kScreenMin.X || m_fMouseX > kScreenMax.X ||
                m_fMouseY < kScreenMin.Y || m_fMouseY > kScreenMax.Y )
        {
            return false;
        }
        Vector3f kTestMin = new Vector3f();       
        Vector3f kTestMax = new Vector3f();       
        if ( m_kDrawingContext.screenToFile( kScreenMin, kTestMin ) || 
                m_kDrawingContext.screenToFile( kScreenMax, kTestMax )  )
        {
            return false;
        }         
        akMinMax[0].Copy( kTestMin );
        akMinMax[1].Copy( kTestMax );
        return true;
    }

    public void move( VOIBase kVOI, Vector3f kDiff )
    {
        if ( kVOI.isFixed() || kDiff.equals( Vector3f.ZERO ) )
        {
            return;
        }
        int iNumPoints = kVOI.size();
        if ( iNumPoints > 0 )
        {
            Vector3f kVolumeDiff = new Vector3f();
            for ( int i = 0; i < iNumPoints; i++ )
            {                
                if ( i == 0 )
                {
                    Vector3f kPos = kVOI.elementAt( i );
                    Vector3f kLocal = m_kDrawingContext.fileToScreen( kPos );
                    kLocal.Add( kDiff );
                    Vector3f kVolumePt = new Vector3f();
                    m_kDrawingContext.screenToFile( (int)kLocal.X, (int)kLocal.Y, (int)kLocal.Z, kVolumePt );
                    kVolumeDiff.Sub( kVolumePt, kPos );
                    kVOI.set( i, kVolumePt );
                }
                else
                {
                    kVOI.elementAt(i).Add(kVolumeDiff);
                }
            }
            kVOI.update(kVolumeDiff);
            if ( kVOI.getGroup().getContourGraph() != null )
            {
                m_kParent.updateGraph(kVOI);
            }
        }
    }

    public boolean nearPoint( VOIBase kVOI, int iX, int iY, int iZ) {

        Vector3f kVOIPoint = new Vector3f(iX, iY, iZ );
        for ( int i = 0; i < kVOI.size(); i++ )
        {
            Vector3f kFilePos = kVOI.get(i);
            Vector3f kPos = m_kDrawingContext.fileToScreen(kFilePos);
            Vector3f kDiff = new Vector3f();
            kDiff.Sub( kPos, kVOIPoint );
            if ( (Math.abs( kDiff.X ) < 3) &&  (Math.abs( kDiff.Y ) < 3) && (Math.abs( kDiff.Z ) < 3) )
            {
                kVOI.setNearPoint(i);
                return true;
            }
        }
        return false;
    }

    public void pasteAllVOI( VOIBase kVOI )
    {
        m_kCopyVOI = kVOI;
        for ( int i = 0; i < m_aiLocalImageExtents[2]; i++ )
        {
            pasteVOI(i);
        }
        m_kCurrentVOI = null;
    }

    public void pasteVOI( VOIBase kVOI )
    {
        m_kCopyVOI = kVOI;
        pasteVOI(m_iSlice);
    }
    public Vector3f patientCoordinatesToFile( Vector3f patientPt )
    {       
        Vector3f volumePt = new Vector3f();
        MipavCoordinateSystems.patientToFile( patientPt, volumePt, m_kImageActive, m_iPlaneOrientation );
        return volumePt;
    }

    public void setActiveImage( int iActive )
    {
        if ( m_akImages[iActive] != null )
        {
            m_kImageActive = m_akImages[iActive];
        }
    }

    private void setCanvas (Component kComponent)
    {
        m_kComponent = kComponent;
        m_kComponent.addKeyListener( this );
        m_kComponent.addMouseListener( this );
        m_kComponent.addMouseMotionListener( this );
    }

    public void setCenter( Vector3f center )
    {
        MipavCoordinateSystems.fileToPatient( center, m_kCenter, m_kImageActive, m_iPlaneOrientation );
        setSlice( m_kCenter.Z );
    }

    public void setCurrentVOI( VOIBase kCurrentVOI )
    {
        m_kCurrentVOI = kCurrentVOI;
    }


    public void setDrawingContext( ScreenCoordinateListener kContext )
    {
        m_kDrawingContext = kContext;
    }



    public void setOrientation( int iOrientation )
    {
        m_iPlaneOrientation = iOrientation;
        m_aiLocalImageExtents = m_kImageActive.getExtents( m_iPlaneOrientation );
        map = new BitSet(m_aiLocalImageExtents[0] * m_aiLocalImageExtents[1]);
    }

    public void setPopupPt( ViewJPopupPt kPopupPt )
    {
        m_kPopupPt = kPopupPt;
    }

    public void setPopupVOI(ViewJPopupVOI kPopupVOI)
    {
        m_kPopupVOI = kPopupVOI;
    }

    /**
     * Sets the local slice value.
     * @param fSlice
     */
    public void setSlice(float fSlice) {
        int iSlice = (int)fSlice;

        /* Check bounds: */
        if ( m_aiLocalImageExtents.length > 3 )
        {
            if (iSlice > (m_aiLocalImageExtents[2] - 1)) {
                iSlice = m_aiLocalImageExtents[2] - 1;
            }
        }
        if (iSlice < 0) {
            iSlice = 0;
        }

        if (iSlice != m_iSlice) {
            m_iSlice = iSlice;
        }
    }


    private void addPopup( VOIBase kVOI )
    {
        if ( kVOI.getType() == VOI.POINT )
        {
            //m_kPopupPt.setSelectedVOI( kVOI );
            m_kComponent.addMouseListener(m_kPopupPt);
        }
        if ( kVOI.getType() == VOI.CONTOUR || kVOI.getType() == VOI.POLYLINE )
        {
            m_kPopupVOI.setSelectedVOI( kVOI );
            m_kComponent.addMouseListener(m_kPopupVOI);
        }
    }






    private void addVOIPoint( int iX, int iY )
    {
        if ( m_kCurrentVOI == null )
        {            
            return;
        }     
        int iPos = m_kCurrentVOI.getNearPoint();
        if ( add( m_kCurrentVOI, iPos, new Vector3f(iX, iY, m_iSlice), false ) )
        {
            m_kParent.setCursor(MipavUtil.crosshairCursor);        
            m_iNearStatus = NearPoint;
            m_kParent.updateDisplay();
        }
    }

    /**
     * Takes a byte and gets appropriate addition from current position.
     *
     * @param   next  Byte to check (0-8).
     *
     * @return  Value to add to current location.
     */
    private int convertGraphToInt(int next, int xDim) {

        switch (next) {

        case 0:
            return (-xDim - 1);

        case 1:
            return (-xDim);

        case 2:
            return (-xDim + 1);

        case 3:
            return (-1);

        case 5:
            return (1);

        case 6:
            return (xDim - 1);

        case 7:
            return (xDim);

        case 8:
            return (xDim + 1);
        }

        return 0;
    }

    private void createTextVOI( int iX, int iY )
    {
        int colorID = 0;
        VOI newTextVOI = new VOI((short) colorID, "annotation3d.voi", 1, VOI.ANNOTATION, -1.0f);

        m_kCurrentVOI = new VOIText( );
        Vector3f kVolumePt = new Vector3f();
        m_kDrawingContext.screenToFile( new Vector3f (iX, iY, m_iSlice), kVolumePt );
        m_kCurrentVOI.add( kVolumePt );

        // decide where to put the second point (arrow tip) so that it is within bounds
        int[] extents = m_aiLocalImageExtents;

        int iX2, iY2;
        if ((iX + 15) < extents[0]) {
            iX2 = iX + 15;
        } else if ((iX - 15) > 0) {
            iX2 = iX - 15;
        } else {
            iX2 = iX;
        }

        if ((iY + 15) < extents[1]) {
            iY2 = iY + 15;
        } else if ((iY - 15) > 0) {
            iY2 = iY - 15;
        } else {
            iY2 = iY;
        }

        kVolumePt = new Vector3f();
        m_kDrawingContext.screenToFile( new Vector3f (iX2, iY2, m_iSlice), kVolumePt );
        m_kCurrentVOI.add( kVolumePt );

        newTextVOI.getCurves().add( m_kCurrentVOI );
        newTextVOI.setUID(newTextVOI.hashCode());

        String prefColor = Preferences.getProperty(Preferences.PREF_VOI_TEXT_COLOR);
        Color textColor;

        if (prefColor != null) {
            textColor = MipavUtil.extractColor(prefColor);
            newTextVOI.setColor(textColor);
        } else {
            Preferences.setProperty(Preferences.PREF_VOI_TEXT_COLOR, MipavUtil.makeColorString(Color.white));
            newTextVOI.setColor(Color.white);
        }
        newTextVOI.setActive(false);
        new JDialogAnnotation(m_kImageActive, newTextVOI, (int)kVolumePt.Z, false, true);
        if ( newTextVOI.isActive() ) {
            m_kParent.addVOI( m_kCurrentVOI, true );
        }
        else
        {
            m_kCurrentVOI = null;
        }

    }

    private VOIBase createVOI( int iType, boolean bClosed, boolean bFixed, Vector<Vector3f> kPositions )
    {

        VOIBase kVOI = null;

        switch( iType )
        {
        case POINT:
            kVOI = new VOIPoint( VOI.POINT );  break;
        case POLYPOINT:
            kVOI = new VOIPoint( VOI.POLYLINE_SLICE ); break;
        case RECTANGLE:
        case RECTANGLE3D:
        case LUT:
        case LEVELSET:
        case OVAL:
        case POLYLINE:
        case LIVEWIRE:
            kVOI = new VOIContour( bFixed, bClosed ); break;
        case LINE:
        case SPLITLINE:
            kVOI = new VOILine(); break;
        case PROTRACTOR:
            kVOI = new VOIProtractor(); ((VOIProtractor)kVOI).setPlane(m_iPlane); break;           
        }

        if ( kVOI == null )
        {
            return null;
        }

        for ( int i = 0; i < kPositions.size(); i++ )
        {
            Vector3f kVolumePt = new Vector3f();
            m_kDrawingContext.screenToFile( kPositions.elementAt(i), kVolumePt );
            kVOI.add( kVolumePt );
        }
        return kVOI;
    }


    private void createVOI( int iX, int iY )
    {
        float fYStart = m_fMouseY;
        float fY = iY;

        VOIBase kOld = m_kCurrentVOI;
        if ( (m_iDrawType == POINT) || (m_iDrawType == POLYPOINT) )
        { 
            Vector<Vector3f> kPositions = new Vector<Vector3f>();
            kPositions.add( new Vector3f (iX, fY, m_iSlice ) );

            m_kCurrentVOI = createVOI( m_iDrawType, false, false, kPositions );

        }
        else if ( m_iDrawType == RECTANGLE || m_iDrawType == RECTANGLE3D || m_iDrawType == LUT )
        {
            if ( m_kCurrentVOI == null )
            {            
                Vector<Vector3f> kPositions = new Vector<Vector3f>();
                kPositions.add( new Vector3f (m_fMouseX, fYStart, m_iSlice));
                kPositions.add( new Vector3f (iX, fYStart, m_iSlice));
                kPositions.add( new Vector3f (iX, fY, m_iSlice));
                kPositions.add( new Vector3f (m_fMouseX, fY, m_iSlice));
                m_kCurrentVOI = createVOI( m_iDrawType, true, false, kPositions );
            }
            else
            {
                setPosition( m_kCurrentVOI, 1, iX, fYStart, m_iSlice);
                setPosition( m_kCurrentVOI, 2, iX, fY, m_iSlice);
                setPosition( m_kCurrentVOI, 3, m_fMouseX, fY, m_iSlice);       
            }
        }
        else if ( m_iDrawType == OVAL )
        {
            float fRadiusX = Math.abs(m_fMouseX - iX);
            float fRadiusY = Math.abs(fYStart - fY);
            if ( m_fMouseX + fRadiusX >= m_aiLocalImageExtents[0] )
            {
                fRadiusX = m_aiLocalImageExtents[0] - m_fMouseX;
            }
            if ( m_fMouseY + fRadiusY >= m_aiLocalImageExtents[1] )
            {
                fRadiusY = m_aiLocalImageExtents[1] - m_fMouseY;
            }
            if ( m_fMouseX - fRadiusX < 0 )
            {
                fRadiusX = m_fMouseX;
            }
            if ( m_fMouseY - fRadiusY < 0 )
            {
                fRadiusY = m_fMouseY;
            }
            if ( m_kCurrentVOI == null )
            {            
                Vector<Vector3f> kPositions = new Vector<Vector3f>();
                for ( int i = 0; i < m_iCirclePts; i++ )
                {
                    kPositions.add( new Vector3f ((float)(m_fMouseX + fRadiusX * m_adCos[i]),
                            (float)(fYStart + fRadiusY * m_adSin[i]), m_iSlice));
                }
                m_kCurrentVOI = createVOI( m_iDrawType, true, false, kPositions );
            }
            else
            {
                for ( int i = 0; i < m_iCirclePts; i++ )
                {
                    setPosition( m_kCurrentVOI, i, (float)(m_fMouseX + fRadiusX * m_adCos[i]),
                            (float)(fYStart + fRadiusY * m_adSin[i]), m_iSlice);
                }
            }
        }
        else if ( m_iDrawType == LEVELSET )
        {
            initLiveWire( m_iSlice, false );
            VOIBase kTemp = singleLevelSet2(iX, fY);
            //VOIBase kTemp = singleLevelSet(iX, fY);
            if ( kTemp == null )
            {
                return;
            }
            m_kCurrentVOI = kTemp;
        }
        else if ( m_iDrawType == POLYLINE )
        {
            if ( m_kCurrentVOI == null )
            {
                Vector<Vector3f> kPositions = new Vector<Vector3f>();
                kPositions.add( new Vector3f( m_fMouseX, fYStart, m_iSlice) ) ;
                kPositions.add( new Vector3f( iX, fY, m_iSlice) ) ;

                m_kCurrentVOI = createVOI( m_iDrawType, false, false, kPositions );
            }
            else
            {
                Vector3f kNewPoint = new Vector3f( iX, fY, m_iSlice ) ;
                if ( add( m_kCurrentVOI, kNewPoint, false ) )
                {
                    m_kCurrentVOI.setAnchor();
                    m_kParent.updateDisplay( );
                }
            }

            m_fMouseX = iX;
            m_fMouseY = iY;
        }
        else if ( (m_iDrawType == LINE) || (m_iDrawType == SPLITLINE) )
        {
            if ( m_kCurrentVOI == null )
            {
                Vector<Vector3f> kPositions = new Vector<Vector3f>();
                kPositions.add( new Vector3f( m_fMouseX, fYStart, m_iSlice) ) ;
                kPositions.add( new Vector3f( iX, fY, m_iSlice) ) ;

                m_kCurrentVOI = createVOI( m_iDrawType, false, false, kPositions );
                if ( m_iDrawType == SPLITLINE )
                {
                    m_kCurrentVOI.setSplit( true);
                }
            }
            else
            {
                Vector3f kNewPoint = new Vector3f( iX, fY, m_iSlice ) ;
                setPosition( m_kCurrentVOI, 1, kNewPoint );
            }

            m_fMouseX = iX;
            m_fMouseY = iY;
        }
        else if ( m_iDrawType == PROTRACTOR )
        {
            if ( m_kCurrentVOI == null )
            {

                Vector3f kStart = new Vector3f( m_fMouseX, fYStart, m_iSlice);
                Vector3f kEnd = new Vector3f( iX, fY, m_iSlice);
                Vector3f kMiddle = new Vector3f();
                kMiddle.Sub( kEnd, kStart );
                kMiddle.Scale( .4f );
                kMiddle.Add(kStart);


                Vector<Vector3f> kPositions = new Vector<Vector3f>();
                kPositions.add( kMiddle );
                kPositions.add( kStart );
                kPositions.add( kEnd );

                m_kCurrentVOI = createVOI( m_iDrawType, false, false, kPositions );
            }
            else
            {
                Vector3f kStart = m_kDrawingContext.fileToScreen(m_kCurrentVOI.get(1));
                Vector3f kEnd = new Vector3f( iX, fY, m_iSlice ) ;
                Vector3f kMiddle = new Vector3f();
                kMiddle.Sub( kEnd, kStart );
                kMiddle.Scale( .4f );
                kMiddle.Add(kStart);

                setPosition( m_kCurrentVOI, 0, kMiddle );
                setPosition( m_kCurrentVOI, 2, kEnd );
            }

            m_fMouseX = iX;
            m_fMouseY = iY;
        }
        m_kCurrentVOI.setActive(false);
        if ( kOld != m_kCurrentVOI )
        {
            m_kParent.addVOI( m_kCurrentVOI, true );
            if ( kOld != null )
            {
                kOld.setActive(false);
                m_kParent.deleteVOI(kOld);
            }
        }
        else
        {
            m_kParent.updateDisplay();
        }
    }

    private void drawArrow( VOIText kVOI, Graphics2D g2d, int xCenter, int yCenter, int x, int y, float stroke) {
        double aDir=Math.atan2(xCenter-x,yCenter-y);

        g2d.setColor(kVOI.getBackgroundColor());
        g2d.drawLine(x + 1, y + 1, xCenter + 1, yCenter + 1);
        g2d.drawLine(x - 1, y - 1, xCenter - 1, yCenter - 1);

        // make the arrow head solid even if dash pattern has been specified
        Polygon tmpPoly=new Polygon();
        Polygon backPoly1 = new Polygon();
        Polygon backPoly2 = new Polygon();
        Polygon backPoly3 = new Polygon();
        Polygon backPoly4 = new Polygon();


        int i1=12+(int)(stroke*2);
        int i2=6+(int)stroke;                           // make the arrow head the same size regardless of the length length
        tmpPoly.addPoint(x,y);                          // arrow tip
        backPoly1.addPoint(x + 1, y);
        backPoly2.addPoint(x - 1, y);
        backPoly3.addPoint(x, y + 1);
        backPoly4.addPoint(x, y - 1);

        int x2 = x+ VOIText.xCor(i1,aDir+.5);
        int y2 = y+ VOIText.yCor(i1,aDir+.5);
        tmpPoly.addPoint(x2, y2);
        backPoly1.addPoint(x2 + 1, y2);
        backPoly2.addPoint(x2 - 1, y2);
        backPoly3.addPoint(x2, y2 + 1);
        backPoly4.addPoint(x2, y2 - 1);


        int x3 = x+ VOIText.xCor(i2,aDir);
        int y3 = y+ VOIText.yCor(i2,aDir);
        tmpPoly.addPoint(x3, y3);
        backPoly1.addPoint(x3 + 1, y3);
        backPoly2.addPoint(x3 - 1, y3);
        backPoly3.addPoint(x3, y3 + 1);
        backPoly4.addPoint(x3, y3 - 1);

        int x4 = x+ VOIText.xCor(i1,aDir-.5);
        int y4 = y+ VOIText.yCor(i1,aDir-.5);
        tmpPoly.addPoint(x4, y4);
        backPoly1.addPoint(x4 + 1, y4 + 1);
        backPoly2.addPoint(x4 - 1, y4 - 1);
        backPoly1.addPoint(x4, y4 + 1);
        backPoly2.addPoint(x4, y4 - 1);

        tmpPoly.addPoint(x,y);                          // arrow tip
        backPoly1.addPoint(x + 1, y + 1);
        backPoly2.addPoint(x - 1, y - 1);
        backPoly3.addPoint(x, y + 1);
        backPoly4.addPoint(x, y - 1);        

        g2d.setStroke(new BasicStroke(1f)); 
        g2d.drawPolygon(backPoly1);
        //g2d.fillPolygon(backPoly1);
        g2d.drawPolygon(backPoly2);
        //g2d.fillPolygon(backPoly2);
        g2d.drawPolygon(backPoly3);
        g2d.drawPolygon(backPoly4);


        g2d.setColor( kVOI.getColor() );

        g2d.drawLine(x,y,xCenter,yCenter);


        g2d.drawPolygon(tmpPoly);
        g2d.fillPolygon(tmpPoly);                       // remove this line to leave arrow head unpainted
    }



    private void drawGeometricCenter(VOIBase kVOI, Graphics g) {
        int xS, yS;

        if (g == null) {
            MipavUtil
            .displayError("VOIContour.drawGeometricCenter: grapics = null");

            return;
        }

        Vector3f gcFilePt = kVOI.getGeometricCenter();
        //Profile.clear();
        //Profile.start();
        //for ( int i = 0; i < 100; i++ )
        //{
        //    kVOI.update();
        //    gcFilePt = kVOI.getGeometricCenter();
        //}
        //Profile.stop();
        //Profile.setFileName( "profile_out" );
        //Profile.shutdown();

        Vector3f gcPt = m_kDrawingContext.fileToScreen( gcFilePt );
        xS = (int)gcPt.X;
        yS = (int)gcPt.Y;
        g.drawLine(xS, yS - 3, xS, yS + 3);
        g.drawLine(xS - 3, yS, xS + 3, yS);

        int iContourID = kVOI.getContourID();
        if ( iContourID != -1 )
        {
            kVOI.setLabel( String.valueOf(iContourID) );
        }

        if (Preferences.is(Preferences.PREF_SHOW_VOI_NAME) && (kVOI.getName() != null)) {
            g.drawString(kVOI.getName(), xS - 10, yS - 5);
        } else if (kVOI.getLabel() != null) {
            g.drawString(kVOI.getLabel(), xS - 10, yS - 5);
        }
    }

    /**
     * Draws the length of open contour (Polyline).
     * 
     * @param g
     *            graphics to draw in
     * @param zoomX
     *            magnification for the x coordinate
     * @param zoomY
     *            magnification for the y coordinate
     * @param unitsOfMeasure
     *            units of measure to be displayed on line.
     * @param res
     *            DOCUMENT ME!
     */
    private void drawLength(Graphics g, VOIBase kVOI, float[] resols, int[] unitsOfMeasure ) {
        String tmpString = kVOI.getTotalLengthString( resols, unitsOfMeasure );

        Vector3f gcFilePt = kVOI.getGeometricCenter();
        Vector3f pt = m_kDrawingContext.fileToScreen( gcFilePt );

        g.setColor(Color.black);
        g.drawString(tmpString, (int) (pt.X), (int) ((pt.Y) - 1));
        g.drawString(tmpString, (int) (pt.X), (int) ((pt.Y) + 1));
        g.drawString(tmpString, (int) ((pt.X) + 1), (int) (pt.Y));
        g.drawString(tmpString, (int) ((pt.X) - 1), (int) (pt.Y));
        g.setColor(Color.white);
        g.drawString(tmpString, (int) (pt.X), (int) (pt.Y));
    }

    private void drawTickMarks(VOIBase kVOI, Graphics g, Color color, int[] unitsOfMeasure, int xD, int yD, float[] res) {
        g.setFont(MipavUtil.font12);

        Vector3f kStart = m_kDrawingContext.fileToScreen( kVOI.get(0) );
        Vector3f kEnd = m_kDrawingContext.fileToScreen( kVOI.get(1) );
        if ( kStart.equals( kEnd ) )
        {
            return;
        }              

        float[] x = new float[2];
        x[0] = kStart.X;
        x[1] = kEnd.X;

        float[] y = new float[2];
        y[0] = kStart.Y;
        y[1] = kEnd.Y;


        double length = MipavMath.length(x, y, res );

        float slope;
        if ((x[1] - x[0]) != 0) {
            slope = (y[1] - y[0]) / (x[1] - x[0]);
        } else {
            slope = Float.MAX_VALUE;
        }

        boolean close = (((y[0] <= (yD / 2)) && (slope < 1) && (slope > -1)) || (x[0] >= (xD / 2)));
        float[] coords = new float[4];
        getCoordsLine(x, y, .5, coords); // get coordinates for tick marks

        // g.setColor(Color.yellow);
        String tmpString = String.valueOf(length);
        int i = tmpString.indexOf('.');

        if (tmpString.length() >= (i + 3)) {
            tmpString = tmpString.substring(0, i + 3);
        }

        tmpString = tmpString + " " + FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[0]);
        int stringX = (int) coords[0];
        int stringY = (int) coords[1];
        boolean drawAngle = Preferences.is(Preferences.PREF_SHOW_LINE_ANGLE);

        double theta = 0;
        if ((x[1] > x[0]) && (y[0] > y[1])) {
            theta = 90.0 - ((180.0 / Math.PI) * Math.atan2((y[0] - y[1]), x[1] - x[0]));
        } else if ((x[1] > x[0]) && (y[1] > y[0])) {
            theta = -(90.0 + ((180.0 / Math.PI) * Math.atan2(y[0] - y[1], x[1] - x[0])));
        } else if ((x[0] > x[1]) && (y[0] > y[1])) {
            theta = -(90.0 - ((180.0 / Math.PI) * Math.atan2(y[0] - y[1], x[0] - x[1])));
        } else if ((x[0] > x[1]) && (y[1] > y[0])) {
            theta = 90.0 - ((180.0 / Math.PI) * Math.atan2(y[1] - y[0], x[0] - x[1]));
        } else if (x[0] == x[1]) {

            // zero angle
            theta = 0;
        } else if (y[0] == y[1]) {

            // 90deg angle
            theta = 90;
        }

        if (drawAngle) {
            String tmpString2 = String.valueOf(theta);
            i = tmpString2.indexOf('.');

            if (tmpString2.length() >= (i + 3)) {
                tmpString2 = tmpString2.substring(0, i + 3);
            }

            tmpString += ", " + tmpString2 + " deg";
        }

        if (close == true) {

            if ((yD - y[0]) < 20) {

                if ((stringY - 21) < 20) {
                    stringY += 45;
                }

                if ((stringX - 21) < 10) {
                    stringX += 25;
                }

                g.setColor(Color.black);
                g.drawString(tmpString, stringX - 20, stringY - 21);
                g.drawString(tmpString, stringX - 20, stringY - 19);
                g.drawString(tmpString, stringX - 21, stringY - 20);
                g.drawString(tmpString, stringX - 19, stringY - 20);
                g.setColor(Color.white);
                g.drawString(tmpString, stringX - 20, stringY - 20);
            } else if ((xD - x[0]) < 20) {
                g.setColor(Color.black);
                g.drawString(tmpString, stringX - 50, stringY + 21);
                g.drawString(tmpString, stringX - 50, stringY + 19);
                g.drawString(tmpString, stringX - 51, stringY + 20);
                g.drawString(tmpString, stringX - 49, stringY + 20);
                g.setColor(Color.white);
                g.drawString(tmpString, stringX - 50, stringY + 20);
            } else {
                g.setColor(Color.black);
                g.drawString(tmpString, stringX - 20, stringY + 21);
                g.drawString(tmpString, stringX - 20, stringY + 19);
                g.drawString(tmpString, stringX - 19, stringY + 20);
                g.drawString(tmpString, stringX - 21, stringY + 20);
                g.setColor(Color.white);
                g.drawString(tmpString, stringX - 20, stringY + 20);
            }
        } else {

            if ((slope > 0) || (slope < -.5)) {
                g.setColor(Color.black);
                g.drawString(tmpString, stringX + 20, stringY + 21);
                g.drawString(tmpString, stringX + 20, stringY + 19);
                g.drawString(tmpString, stringX + 21, stringY + 20);
                g.drawString(tmpString, stringX + 19, stringY + 20);
                g.setColor(Color.white);
                g.drawString(tmpString, stringX + 20, stringY + 20);
            } else {
                g.setColor(Color.black);
                g.drawString(tmpString, stringX - 40, stringY - 21);
                g.drawString(tmpString, stringX - 40, stringY - 19);
                g.drawString(tmpString, stringX - 41, stringY - 20);
                g.drawString(tmpString, stringX - 39, stringY - 20);
                g.setColor(Color.white);
                g.drawString(tmpString, stringX - 40, stringY - 20);
            }
        }

        g.setColor(color);
        g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        getCoordsLine(x, y, .25, coords);
        g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        getCoordsLine(x, y, .75, coords);
        g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        g.setColor(color);

        for (i = 0; i < 4; i++) {
            getEndLinesLine(x, y, i, coords);
            g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        }
    }



    private void drawTickMarks( VOIBase kVOI, Graphics g, int[] unitsOfMeasure, int xD, int yD, float[] res)
    {
        int i;
        double slope;
        boolean close;
        float[] x2 = new float[2];
        float[] y2 = new float[2];

        if (g == null) {
            MipavUtil.displayError("VOIprotractor drawTickMarks: graphics = null");

            return;
        }

        Color currentColor = g.getColor();

        g.setPaintMode();
        g.setFont(MipavUtil.font12);

        Vector3f kStart = m_kDrawingContext.fileToScreen( kVOI.get(0) );
        Vector3f kMiddle = m_kDrawingContext.fileToScreen( kVOI.get(1) );
        Vector3f kEnd = m_kDrawingContext.fileToScreen( kVOI.get(2) );


        if ( kStart.equals( kEnd ) )
        {
            return;
        }

        //0 is middle, 1 is start, 2 is end:
        float[] x = new float[3];
        x[0] = kMiddle.X;
        x[1] = kStart.X;
        x[2] = kEnd.X;

        //0 is middle, 1 is start, 2 is end:
        float[] y = new float[3];
        y[0] = kMiddle.Y;
        y[1] = kStart.Y;
        y[2] = kEnd.Y;

        if ((x[1] - x[0]) != 0) {
            slope = (y[1] - y[0]) / (x[1] - x[0]);
        } else {
            slope = Double.MAX_VALUE;
        }

        close = (((y[0] <= (yD / 2)) && (slope < 1) && (slope > -1)) || (x[0] >= (xD / 2)));
        float[] coords = new float[4];
        getCoordsProtractor(x, y, .5, coords); // get coordinates for tick marks

        String degreeString = ((VOIProtractor)kVOI).getAngleString( res );

        if (close == true) {

            if ((yD - y[0]) < 20) {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] - 21);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] - 19);
                g.drawString(degreeString, (int) coords[0] - 21, (int) coords[1] - 20);
                g.drawString(degreeString, (int) coords[0] - 19, (int) coords[1] - 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] - 20);
            } else if ((xD - x[0]) < 20) {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] - 50, (int) coords[1] + 21);
                g.drawString(degreeString, (int) coords[0] - 50, (int) coords[1] + 19);
                g.drawString(degreeString, (int) coords[0] - 51, (int) coords[1] + 20);
                g.drawString(degreeString, (int) coords[0] - 49, (int) coords[1] + 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] - 50, (int) coords[1] + 20);
            } else {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] + 21);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] + 19);
                g.drawString(degreeString, (int) coords[0] - 19, (int) coords[1] + 20);
                g.drawString(degreeString, (int) coords[0] - 21, (int) coords[1] + 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] + 20);
            }
        } else {

            if ((slope > 0) || (slope < -.5)) {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] + 20, (int) coords[1] + 21);
                g.drawString(degreeString, (int) coords[0] + 20, (int) coords[1] + 19);
                g.drawString(degreeString, (int) coords[0] + 21, (int) coords[1] + 20);
                g.drawString(degreeString, (int) coords[0] + 19, (int) coords[1] + 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] + 20, (int) coords[1] + 20);
            } else {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] - 40, (int) coords[1] - 21);
                g.drawString(degreeString, (int) coords[0] - 40, (int) coords[1] - 19);
                g.drawString(degreeString, (int) coords[0] - 41, (int) coords[1] - 20);
                g.drawString(degreeString, (int) coords[0] - 39, (int) coords[1] - 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] - 40, (int) coords[1] - 20);
            }
        }

        g.setColor( currentColor );

        for (i = 0; i < 2; i++) {
            getEndLinesProtractor(x, y, i, coords);
            g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        }

        if ((x[2] - x[0]) != 0) {
            slope = (y[2] - y[0]) / (x[2] - x[0]);
        } else {
            slope = Double.MAX_VALUE;
        }

        close = (((y[2] <= (yD / 2)) && (slope < 1) && (slope > -1)) || (x[2] >= (xD / 2)));
        x2[0] = x[0];
        x2[1] = x[2];
        y2[0] = y[0];
        y2[1] = y[2];
        getCoordsProtractor(x2, y2, .5, coords); // get coordinates for tick marks

        boolean showLengths = true;
        if (showLengths) {

            String lengthString = kVOI.getLengthString( 0, 1, res, unitsOfMeasure );
            if (close == true) {

                if ((yD - y2[1]) < 20) {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] - 21);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] - 19);
                    g.drawString(lengthString, (int) coords[0] - 21, (int) coords[1] - 20);
                    g.drawString(lengthString, (int) coords[0] - 19, (int) coords[1] - 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] - 20);
                } else if ((xD - x2[1]) < 20) {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] - 50, (int) coords[1] + 21);
                    g.drawString(lengthString, (int) coords[0] - 50, (int) coords[1] + 19);
                    g.drawString(lengthString, (int) coords[0] - 51, (int) coords[1] + 20);
                    g.drawString(lengthString, (int) coords[0] - 49, (int) coords[1] + 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] - 50, (int) coords[1] + 20);
                } else {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] + 21);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] + 19);
                    g.drawString(lengthString, (int) coords[0] - 19, (int) coords[1] + 20);
                    g.drawString(lengthString, (int) coords[0] - 21, (int) coords[1] + 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] + 20);
                }
            } else {

                if ((slope > 0) || (slope < -.5)) {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] + 20, (int) coords[1] + 21);
                    g.drawString(lengthString, (int) coords[0] + 20, (int) coords[1] + 19);
                    g.drawString(lengthString, (int) coords[0] + 21, (int) coords[1] + 20);
                    g.drawString(lengthString, (int) coords[0] + 19, (int) coords[1] + 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] + 20, (int) coords[1] + 20);
                } else {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] - 40, (int) coords[1] - 21);
                    g.drawString(lengthString, (int) coords[0] - 40, (int) coords[1] - 19);
                    g.drawString(lengthString, (int) coords[0] - 41, (int) coords[1] - 20);
                    g.drawString(lengthString, (int) coords[0] - 39, (int) coords[1] - 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] - 40, (int) coords[1] - 20);
                }
            }
        } // end of if (showLengths)

        g.setColor( currentColor );

        for (i = 0; i < 2; i++) {
            getEndLinesProtractor(x2, y2, i, coords);
            g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        }
    }


    /**
     * Draws the vertices of the contour.
     */
    private void drawVertices( VOIBase kVOI, float[] resols, int[] unitsOfMeasure, Graphics g, int thickness, boolean boundingBox) {
        Polygon gon = null;
        int j;

        if (g == null) {
            MipavUtil.displayError("VOIContour.drawSelf: grapics = null");

            return;
        }

        gon = scalePolygon(kVOI);

        // if active draw little boxes at points
        if ( kVOI.isActive() ) {

            // drawCenterOfMass(scaleX, scaleY, g);
            for (j = 0; j < kVOI.size(); j++) {

                if (kVOI.getNearPoint() != j) { // Highlight Active point
                    g.setColor(Color.white);
                    g.fillRect((int) (gon.xpoints[j] - 1.5 + 0.5f),
                            (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                    g.setColor(Color.black);
                    g.drawRect((int) (gon.xpoints[j] - 1.5 + 0.5f),
                            (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                }
            }

            g.setColor(Color.yellow);
            g.drawRect((int) (gon.xpoints[0] - 1.5 + 0.5f),
                    (int) (gon.ypoints[0] - 1.5 + 0.5f), 3, 3);
        }
        /*
        if (boundingBox == true) {
            int x0, x1, y0, y1;
            x0 = (int) ((xBounds[0] * zoomX * resolutionX) + 0.5f);
            x1 = (int) ((xBounds[1] * zoomX * resolutionX) + 0.5f);
            y0 = (int) ((yBounds[0] * zoomY * resolutionY) + 0.5f);
            y1 = (int) ((yBounds[1] * zoomY * resolutionY) + 0.5f);
            g.setColor(Color.yellow.darker());
            g.drawRect(x0, y0, x1 - x0, y1 - y0);

            // draw corners of bounding box to make handles for resizing VOI
            g.fillRect(x0 - 2, y0 - 2, 5, 5);
            g.fillRect(x1 - 2, y0 - 2, 5, 5);
            g.fillRect(x0 - 2, y1 - 2, 5, 5);
            g.fillRect(x1 - 2, y1 - 2, 5, 5);

            // draw mid points of bounding box to make handles for resizing VOI
            g.fillRect(MipavMath.round(x0 + ((x1 - x0) / 2) - 2), y0 - 2, 5, 5);
            g.fillRect(x1 - 2, Math.round(y0 + ((y1 - y0) / 2) - 2), 5, 5);
            g.fillRect(MipavMath.round(x0 + ((x1 - x0) / 2) - 2), y1 - 2, 5, 5);
            g.fillRect(x0 - 2, Math.round(y0 + ((y1 - y0) / 2) - 2), 5, 5);
            g.setColor(Color.yellow.brighter());

            switch (nearBoundPoint) {

            case 1:
                g.fillRect(x0 - 2, y0 - 2, 5, 5);
                g.setColor(Color.black);
                g.drawRect(x0 - 2, y0 - 2, 4, 4);
                break;

            case 2:
                g.fillRect(x1 - 2, y0 - 2, 5, 5);
                g.setColor(Color.black);
                g.drawRect(x1 - 2, y0 - 2, 4, 4);
                break;

            case 3:
                g.fillRect(x1 - 2, y1 - 2, 5, 5);
                g.setColor(Color.black);
                g.drawRect(x1 - 2, y1 - 2, 4, 4);
                break;

            case 4:
                g.fillRect(x0 - 2, y1 - 2, 5, 5);
                g.setColor(Color.black);
                g.drawRect(x0 - 2, y1 - 2, 4, 4);
                break;

            case 5:
                g.fillRect(MipavMath.round(x0 + ((x1 - x0) / 2) - 2), y0 - 2,
                        5, 5);
                g.setColor(Color.black);
                g.drawRect(MipavMath.round(x0 + ((x1 - x0) / 2) - 2), y0 - 2,
                        4, 4);
                break;

            case 6:
                g.fillRect(x1 - 2, MipavMath.round(y0 + ((y1 - y0) / 2) - 2),
                        5, 5);
                g.setColor(Color.black);
                g.drawRect(x1 - 2, MipavMath.round(y0 + ((y1 - y0) / 2) - 2),
                        4, 4);
                break;

            case 7:
                g.fillRect(MipavMath.round(x0 + ((x1 - x0) / 2) - 2), y1 - 2,
                        5, 5);
                g.setColor(Color.black);
                g.drawRect(MipavMath.round(x0 + ((x1 - x0) / 2) - 2), y1 - 2,
                        4, 4);
                break;

            case 8:
                g.fillRect(x0 - 2, MipavMath.round(y0 + ((y1 - y0) / 2) - 2),
                        5, 5);
                g.setColor(Color.black);
                g.drawRect(x0 - 2, MipavMath.round(y0 + ((y1 - y0) / 2) - 2),
                        4, 4);
                break;
            }
        }
         */
    }

    public Vector3f drawBlendContour( VOIBase kVOI, int[] pixBuffer, float opacity, Color color, int slice )
    {
        if ( (m_iPlane != (m_iPlane & kVOI.getPlane())) || (getSlice(kVOI)!= slice) )
        {
            return null;
        }
        int iNumPoints = kVOI.size();
        if ( iNumPoints == 0 )
        {
            return null;
        }
        Vector3f[] kVolumePts = new Vector3f[iNumPoints + 1];
        int iXMin = Integer.MAX_VALUE;
        int iYMin = Integer.MAX_VALUE;
        int iXMax = Integer.MIN_VALUE;
        int iYMax = Integer.MIN_VALUE;

        for ( int i = 0; i < iNumPoints; i++ )
        {
            Vector3f kVolumePt = kVOI.get(i);
            Vector3f kPt = fileCoordinatesToPatient( kVolumePt );

            //kPt.Z = iSlice;/
            kVolumePts[i] = kPt;
            iXMin = (int)Math.min( iXMin, kVolumePts[i].X );
            iYMin = (int)Math.min( iYMin, kVolumePts[i].Y );
            iXMax = (int)Math.max( iXMax, kVolumePts[i].X );
            iYMax = (int)Math.max( iYMax, kVolumePts[i].Y );
        }
        Vector3f kVolumePt = kVOI.get(0);
        Vector3f kPt = fileCoordinatesToPatient( kVolumePt );

        //Vector3f kPt = kPoly.VBuffer.GetPosition3(0);
        //kPt.Mult( m_kVolumeScaleInv );
        //kPt.Z = iSlice;
        kVolumePts[iNumPoints] = kPt;
        iXMin = (int)Math.min( iXMin, kVolumePts[iNumPoints].X );
        iYMin = (int)Math.min( iYMin, kVolumePts[iNumPoints].Y );
        iXMax = (int)Math.max( iXMax, kVolumePts[iNumPoints].X );
        iYMax = (int)Math.max( iYMax, kVolumePts[iNumPoints].Y );
        iNumPoints++;

        int[][] aaiCrossingPoints = new int[iXMax - iXMin + 1][];
        int[] aiNumCrossings = new int[iXMax - iXMin + 1];

        for (int i = 0; i < (iXMax - iXMin + 1); i++) {
            aaiCrossingPoints[i] = new int[iNumPoints];
        }

        VOIBase.outlineRegion(aaiCrossingPoints, aiNumCrossings, iXMin, iYMin, iXMax, iYMax, kVolumePts);


        int numPts = 0;
        Vector3f kCenterMass = new Vector3f();
        Vector3f kLocalPt = new Vector3f();
        int iColumn = 0;
        /* Loop over the width of the sculpt region bounding-box: */
        for (int iX = iXMin; iX < iXMax; iX++) {
            boolean bInside = false;

            /* Loop over the height of the sculpt region bounding-box: */
            for (int iY = iYMin; iY < iYMax; iY++) {

                /* loop over each crossing point for this column: */
                for (int iCross = 0; iCross < aiNumCrossings[iColumn]; iCross++) {

                    if (iY == aaiCrossingPoints[iColumn][iCross]) {

                        /* Each time an edge is cross the point alternates
                         * from outside to inside: */
                        bInside = !bInside;
                    }
                }

                if (bInside == true) {

                    /* The current pixel is inside the sculpt region.  Get the
                     * image color from the canvas image and alpha-blend the sculpt color ontop, storing the result in
                     * the canvas image.
                     */
                    if ( pixBuffer != null )
                    {
                        int index = iY * m_aiLocalImageExtents[0] + iX;
                        int opacityInt = (int) (opacity * 255);
                        opacityInt = opacityInt << 24;

                        int colorInt = color.getRGB() & 0x00ffffff;
                        pixBuffer[index] = colorInt | opacityInt;
                    }
                    else
                    {
                        kCenterMass.Add( patientCoordinatesToFile( new Vector3f( iX, iY, m_iSlice ) ) );
                        numPts++;
                    }
                }
            }

            iColumn++;
        }      
        kCenterMass.X = MipavMath.round( kCenterMass.X / numPts );
        kCenterMass.Y = MipavMath.round( kCenterMass.Y / numPts );
        kCenterMass.Z = MipavMath.round( kCenterMass.Z / numPts );
        return kCenterMass;
    }

    private void drawVOI( VOIBase kVOI, float[] resols, int[] unitsOfMeasure, Graphics g, int thickness ) {
        Polygon gon = null;
        int j;
        new DecimalFormat(".##");

        gon = scalePolygon(kVOI);

        if (kVOI.isActive()) {
            if (kVOI.isClosed()) {
                drawGeometricCenter(kVOI, g);
            } else {
                drawLength( g, kVOI, resols, unitsOfMeasure );
            }
        } else if ( kVOI.getDoGeometricCenterLabel() && kVOI.isClosed()) {
            drawGeometricCenter(kVOI, g);
        }

        if ( thickness == 1) {
            if (kVOI.isClosed() == true) {
                g.drawPolygon(gon);

            } else {
                g.drawPolyline(gon.xpoints, gon.ypoints, gon.npoints);
            }
        }  else {
            // thickness is greater than 1... must draw differently

            int x1, x2, y1, y2;
            int dX, dY, dx, dy;
            double ddx, ddy, lineLength, scale;

            for (int i = 0; i < kVOI.size() - 1; i++) {

                Vector3f kVolumePt = kVOI.elementAt(i);
                Vector3f kScreen = m_kDrawingContext.fileToScreen( kVolumePt );                
                x1 = (int) kScreen.X;
                y1 = (int) kScreen.Y;

                kVolumePt = kVOI.elementAt(i+1);
                kScreen = m_kDrawingContext.fileToScreen( kVolumePt );                
                x2 = (int) kScreen.X;
                y2 = (int) kScreen.Y;

                // now draw the connecting lines as polygons with thickness
                dX = x2 - x1;
                dY = y2 - y1;
                // line length
                lineLength = Math.sqrt(dX * dX + dY * dY);

                scale = (thickness) / (2 * lineLength);

                // The x,y increments from an endpoint needed to create a
                // rectangle...
                ddx = -scale * dY;
                ddy = scale * dX;
                ddx += (ddx > 0) ? 0.5 : -0.5;
                ddy += (ddy > 0) ? 0.5 : -0.5;
                dx = (int) ddx;
                dy = (int) ddy;

                // Now we can compute the corner points...
                int xPoints[] = new int[4];
                int yPoints[] = new int[4];

                xPoints[0] = x1 + dx;
                yPoints[0] = y1 + dy;
                xPoints[1] = x1 - dx;
                yPoints[1] = y1 - dy;
                xPoints[2] = x2 - dx;
                yPoints[2] = y2 - dy;
                xPoints[3] = x2 + dx;
                yPoints[3] = y2 + dy;

                g.fillPolygon(xPoints, yPoints, 4);
            }
            // if it's closed... connect the last and first points
            if ( kVOI.isClosed() ) {
                Vector3f kVolumePt = kVOI.elementAt( kVOI.size() - 1);
                Vector3f kScreen = m_kDrawingContext.fileToScreen( kVolumePt );                
                x1 = (int) kScreen.X;
                y1 = (int) kScreen.Y;

                kVolumePt = kVOI.elementAt(0);
                kScreen = m_kDrawingContext.fileToScreen( kVolumePt );                
                x2 = (int) kScreen.X;
                y2 = (int) kScreen.Y;

                // now draw the connecting lines as polygons with thickness
                dX = x2 - x1;
                dY = y2 - y1;
                // line length
                lineLength = Math.sqrt(dX * dX + dY * dY);

                scale = (thickness) / (2 * lineLength);

                // The x,y increments from an endpoint needed to create a
                // rectangle...
                ddx = -scale * dY;
                ddy = scale * dX;
                ddx += (ddx > 0) ? 0.5 : -0.5;
                ddy += (ddy > 0) ? 0.5 : -0.5;
                dx = (int) ddx;
                dy = (int) ddy;

                // Now we can compute the corner points...
                int xPoints[] = new int[4];
                int yPoints[] = new int[4];

                xPoints[0] = x1 + dx;
                yPoints[0] = y1 + dy;
                xPoints[1] = x1 - dx;
                yPoints[1] = y1 - dy;
                xPoints[2] = x2 - dx;
                yPoints[2] = y2 - dy;
                xPoints[3] = x2 + dx;
                yPoints[3] = y2 + dy;

                g.fillPolygon(xPoints, yPoints, 4);
            }

        }


        if (kVOI.isActive() /*&& getSType() != VOIManager.LEVELSET
                && getSType() != VOIManager.LIVEWIRE */) {
            // if active draw little boxes at points
            for (j = 0; j < kVOI.size(); j++) {

                if ( kVOI.getSelectedPoint() == j) { // Do not draw (dragging point)
                } else {
                    g.setColor(Color.white);
                    g.fillRect((int) (gon.xpoints[j] - 1.5 + 0.5f),
                            (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                    g.setColor(Color.black);
                    g.drawRect((int) (gon.xpoints[j] - 1.5 + 0.5f),
                            (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                }
            }

            // draw the 1st point only if not dragging the first point and if
            // the active point (lastPoint)
            // is not the first point
            if (kVOI.getSelectedPoint() != 0) {
                g.setColor(Color.yellow);
                g.drawRect((int) (gon.xpoints[0] - 1.5 + 0.5f),
                        (int) (gon.ypoints[0] - 1.5 + 0.5f), 3, 3);
            }
            // draw the active point dragging is taking place
            if ((kVOI.getSelectedPoint() >= 0) && (kVOI.size() > kVOI.getSelectedPoint())) {
                g.setColor(Color.GREEN);
                g.fillRect((int) (gon.xpoints[kVOI.getSelectedPoint()] - 1.5 + 0.5f),
                        (int) (gon.ypoints[kVOI.getSelectedPoint()] - 1.5 + 0.5f), 3, 3);
            }                       

            /*
            if (boundingBox == true) {
                int x0, x1, y0, y1;
                x0 = (int) ((xBounds[0] * zoomX * resolutionX) + 0.5);
                x1 = (int) ((xBounds[1] * zoomX * resolutionX) + 0.5);
                y0 = (int) ((yBounds[0] * zoomY * resolutionY) + 0.5);
                y1 = (int) ((yBounds[1] * zoomY * resolutionY) + 0.5);
                g.setColor(Color.yellow.darker());
                g.drawRect(x0, y0, x1 - x0, y1 - y0);

                // draw corners of bounding box to make handles for resizing VOI
                g.fillRect(x0 - 2, y0 - 2, 5, 5);
                g.fillRect(x1 - 2, y0 - 2, 5, 5);
                g.fillRect(x0 - 2, y1 - 2, 5, 5);
                g.fillRect(x1 - 2, y1 - 2, 5, 5);

                // draw mid points of bounding box to make handles for resizing
                // VOI
                g.fillRect(x0 + ((x1 - x0) / 2) - 2, y0 - 2, 5, 5);
                g.fillRect(x1 - 2, y0 + ((y1 - y0) / 2) - 2, 5, 5);
                g.fillRect(x0 + ((x1 - x0) / 2) - 2, y1 - 2, 5, 5);
                g.fillRect(x0 - 2, y0 + ((y1 - y0) / 2) - 2, 5, 5);

                // display the height/width of the bounding box above (or below)
                // the top
                // midpoint and to the right of (or left of) the right midpoint
                String widthString, heightString;
                String measuredWidthString, measuredHeightString;
                String upperLeftLocationString;
                String lowerXYmmString;
                int width = (int) ((xBounds[1] - xBounds[0]) + 0.5f);
                int height = (int) ((yBounds[1] - yBounds[0]) + 0.5f);
                widthString = String.valueOf(width);
                heightString = String.valueOf(height);
                measuredWidth = (xBounds[1] - xBounds[0]) * resols[0];
                measuredHeight = (yBounds[1] - yBounds[0]) * resols[1];
                xUnitsString = FileInfoBase
                        .getUnitsOfMeasureAbbrevStr(unitsOfMeasure[0]);
                yUnitsString = FileInfoBase
                        .getUnitsOfMeasureAbbrevStr(unitsOfMeasure[1]);
                measuredWidthString = String.valueOf(nf.format(measuredWidth))
                        + " " + xUnitsString;
                measuredHeightString = String
                        .valueOf(nf.format(measuredHeight))
                        + " " + yUnitsString;

                // System.err.println("width: " + widthString + " height: " +
                // heightString);
                float lowerXmm = originX + (resols[0] * xBounds[0]);
                float lowerYmm = originY + (resols[1] * yBounds[0]);
                lowerXYmmString = "(" + String.valueOf(nf.format(lowerXmm))
                        + " " + xUnitsString + ", "
                        + String.valueOf(nf.format(lowerYmm)) + " "
                        + yUnitsString + ")";
                upperLeftLocationString = "("
                        + String.valueOf((int) (xBounds[0] + 0.5f)) + ","
                        + String.valueOf((int) (yBounds[0] + 0.5f)) + ")";
                g.setColor(Color.black);

                // System.err.println(xBounds[0] + " " + xBounds[1] + " " +
                // yBounds[0] + " " + yBounds[1]);
                if ((y1 - 45) < 0) {
                    g.drawString(measuredWidthString,
                            (x0 + ((x1 - x0) / 2)) - 20, y1 + 21);
                    g.drawString(measuredWidthString,
                            (x0 + ((x1 - x0) / 2)) - 20, y1 + 19);
                    g.drawString(measuredWidthString,
                            (x0 + ((x1 - x0) / 2)) - 21, y1 + 20);
                    g.drawString(measuredWidthString,
                            (x0 + ((x1 - x0) / 2)) - 19, y1 + 20);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20,
                            y1 + 36);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20,
                            y1 + 34);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 21,
                            y1 + 35);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 19,
                            y1 + 35);
                    g.setColor(Color.white);
                    g.drawString(measuredWidthString,
                            (x0 + ((x1 - x0) / 2)) - 20, y1 + 20);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20,
                            y1 + 35);
                } else {
                    g.drawString(measuredWidthString,
                            (x0 + ((x1 - x0) / 2)) - 20, y1 - 24);
                    g.drawString(measuredWidthString,
                            (x0 + ((x1 - x0) / 2)) - 20, y1 - 26);
                    g.drawString(measuredWidthString,
                            (x0 + ((x1 - x0) / 2)) - 21, y1 - 25);
                    g.drawString(measuredWidthString,
                            (x0 + ((x1 - x0) / 2)) - 19, y1 - 25);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20,
                            y1 - 9);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20,
                            y1 - 11);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 21,
                            y1 - 10);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 19,
                            y1 - 10);
                    g.setColor(Color.white);
                    g.drawString(measuredWidthString,
                            (x0 + ((x1 - x0) / 2)) - 20, y1 - 25);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20,
                            y1 - 10);
                }

                g.setColor(Color.black);

                if ((x0 - 40) < 0) {
                    g.drawString(measuredHeightString, x0 + 10, y0 + 10
                            + ((y1 - y0) / 2) + 1);
                    g.drawString(measuredHeightString, x0 + 10, y0 + 10
                            + ((y1 - y0) / 2) - 1);
                    g.drawString(measuredHeightString, x0 + 9, y0 + 10
                            + ((y1 - y0) / 2));
                    g.drawString(measuredHeightString, x0 + 11, y0 + 10
                            + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 + 10, y0 + 25
                            + ((y1 - y0) / 2) + 1);
                    g.drawString(heightString, x0 + 10, y0 + 25
                            + ((y1 - y0) / 2) - 1);
                    g.drawString(heightString, x0 + 9, y0 + 25
                            + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 + 11, y0 + 25
                            + ((y1 - y0) / 2));
                    g.setColor(Color.white);
                    g.drawString(measuredHeightString, x0 + 10, y0 + 10
                            + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 + 10, y0 + 25
                            + ((y1 - y0) / 2));
                } else {
                    g.drawString(measuredHeightString, x0 - 35, y0 + 10
                            + ((y1 - y0) / 2) + 1);
                    g.drawString(measuredHeightString, x0 - 35, y0 + 10
                            + ((y1 - y0) / 2) - 1);
                    g.drawString(measuredHeightString, x0 - 36, y0 + 10
                            + ((y1 - y0) / 2));
                    g.drawString(measuredHeightString, x0 - 34, y0 + 10
                            + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 - 35, y0 + 25
                            + ((y1 - y0) / 2) + 1);
                    g.drawString(heightString, x0 - 35, y0 + 25
                            + ((y1 - y0) / 2) - 1);
                    g.drawString(heightString, x0 - 36, y0 + 25
                            + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 - 34, y0 + 25
                            + ((y1 - y0) / 2));
                    g.setColor(Color.white);
                    g.drawString(measuredHeightString, x0 - 35, y0 + 10
                            + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 - 35, y0 + 25
                            + ((y1 - y0) / 2));
                }

                g.setColor(Color.black);

                if (((x0 - 40) <= 0) && ((y0 - 45) <= 0)) {
                    g.drawString(lowerXYmmString, x0 + 10, y0 + 11);
                    g.drawString(lowerXYmmString, x0 + 10, y0 + 13);
                    g.drawString(lowerXYmmString, x0 + 9, y0 + 12);
                    g.drawString(lowerXYmmString, x0 + 11, y0 + 12);
                    g.setColor(Color.white);
                    g.drawString(lowerXYmmString, x0 + 10, y0 + 12);
                    g.setColor(Color.black);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 + 29);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 + 17);
                    g.drawString(upperLeftLocationString, x0 + 9, y0 + 28);
                    g.drawString(upperLeftLocationString, x0 + 11, y0 + 28);
                    g.setColor(Color.white);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 + 28);
                } else if (((x0 - 40) <= 0) && ((y0 - 45) > 0)) {
                    g.drawString(lowerXYmmString, x0 + 10, y0 - 25);
                    g.drawString(lowerXYmmString, x0 + 10, y0 - 27);
                    g.drawString(lowerXYmmString, x0 + 9, y0 - 26);
                    g.drawString(lowerXYmmString, x0 + 11, y0 - 26);
                    g.setColor(Color.white);
                    g.drawString(lowerXYmmString, x0 + 10, y0 - 26);
                    g.setColor(Color.black);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 - 9);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 - 21);
                    g.drawString(upperLeftLocationString, x0 + 9, y0 - 10);
                    g.drawString(upperLeftLocationString, x0 + 11, y0 - 10);
                    g.setColor(Color.white);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 - 10);
                } else if (((x0 - 40) > 0) && ((y0 - 45) <= 0)) {
                    g.drawString(lowerXYmmString, x0 - 35, y0 + 11);
                    g.drawString(lowerXYmmString, x0 - 35, y0 + 13);
                    g.drawString(lowerXYmmString, x0 - 36, y0 + 12);
                    g.drawString(lowerXYmmString, x0 - 34, y0 + 12);
                    g.setColor(Color.white);
                    g.drawString(lowerXYmmString, x0 - 35, y0 + 12);
                    g.setColor(Color.black);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 + 29);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 + 17);
                    g.drawString(upperLeftLocationString, x0 - 36, y0 + 28);
                    g.drawString(upperLeftLocationString, x0 - 34, y0 + 28);
                    g.setColor(Color.white);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 + 28);
                } else {
                    g.drawString(lowerXYmmString, x0 - 35, y0 - 25);
                    g.drawString(lowerXYmmString, x0 - 35, y0 - 27);
                    g.drawString(lowerXYmmString, x0 - 36, y0 - 26);
                    g.drawString(lowerXYmmString, x0 - 34, y0 - 26);
                    g.setColor(Color.white);
                    g.drawString(lowerXYmmString, x0 - 35, y0 - 26);
                    g.setColor(Color.black);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 - 9);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 - 11);
                    g.drawString(upperLeftLocationString, x0 - 36, y0 - 10);
                    g.drawString(upperLeftLocationString, x0 - 34, y0 - 10);
                    g.setColor(Color.white);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 - 10);
                }

                // System.err.println("height: " + heightString + " width: " +
                // widthString);
                g.setColor(Color.yellow.brighter());

                switch (nearBoundPoint) {

                case 1:
                    g.fillRect(x0 - 2, y0 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x0 - 2, y0 - 2, 4, 4);
                    break;

                case 2:
                    g.fillRect(x1 - 2, y0 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x1 - 2, y0 - 2, 4, 4);
                    break;

                case 3:
                    g.fillRect(x1 - 2, y1 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x1 - 2, y1 - 2, 4, 4);
                    break;

                case 4:
                    g.fillRect(x0 - 2, y1 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x0 - 2, y1 - 2, 4, 4);
                    break;

                case 5:
                    g.fillRect(x0 + ((x1 - x0) / 2) - 2, y0 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x0 + ((x1 - x0) / 2) - 2, y0 - 2, 4, 4);
                    break;

                case 6:
                    g.fillRect(x1 - 2, y0 + ((y1 - y0) / 2) - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x1 - 2, y0 + ((y1 - y0) / 2) - 2, 4, 4);
                    break;

                case 7:
                    g.fillRect(x0 + ((x1 - x0) / 2) - 2, y1 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x0 + ((x1 - x0) / 2) - 2, y1 - 2, 4, 4);
                    break;

                case 8:
                    g.fillRect(x0 - 2, y0 + ((y1 - y0) / 2) - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x0 - 2, y0 + ((y1 - y0) / 2) - 2, 4, 4);
                    break;
                }
            }
             */
        }
    }




    private void drawVOILine( VOIBase kVOI, float[] resols, int[] unitsOfMeasure, Graphics g, int thickness  ) {

        Vector3f kStart = m_kDrawingContext.fileToScreen( kVOI.get(0) );
        Vector3f kEnd = m_kDrawingContext.fileToScreen( kVOI.get(1) );
        float[] x = new float[2];
        x[0] = kStart.X;
        x[1] = kEnd.X;

        float[] y = new float[2];
        y[0] = kStart.Y;
        y[1] = kEnd.Y;


        MipavMath.length(x, y, resols );

        if (thickness == 1) {
            g.drawLine((int) x[0], (int) y[0], (int) x[1], (int) y[1]);
        } else {

            int dX = (int) (x[1] - x[0]);
            int dY = (int) (y[1] - y[0]);
            // line length
            double lineLength = Math.sqrt(dX * dX + dY * dY);

            double scale = (thickness) / (2 * lineLength);

            // The x,y increments from an endpoint needed to create a rectangle...
            double ddx = -scale * dY;
            double ddy = scale * dX;
            ddx += (ddx > 0) ? 0.5 : -0.5;
            ddy += (ddy > 0) ? 0.5 : -0.5;
            int dx = (int)ddx;
            int dy = (int)ddy;

            // Now we can compute the corner points...
            int xPoints[] = new int[4];
            int yPoints[] = new int[4];

            xPoints[0] = (int)x[0] + dx; yPoints[0] = (int)y[0] + dy;
            xPoints[1] = (int)x[0] - dx; yPoints[1] = (int)y[0] - dy;
            xPoints[2] = (int)x[1] - dx; yPoints[2] = (int)y[1] - dy;
            xPoints[3] = (int)x[1] + dx; yPoints[3] = (int)y[1] + dy;

            g.fillPolygon(xPoints, yPoints, 4);



        }

        Color currentColor = g.getColor();

        if ( kVOI.isActive() ) {
            // draw the active point dragging is taking place
            if (( kVOI.getSelectedPoint() >= 0) && (kVOI.size() > kVOI.getSelectedPoint())) {
                g.setColor(Color.GREEN);
                g.fillRect((int) (x[kVOI.getSelectedPoint()] - 1.5 + 0.5f), (int) (y[kVOI.getSelectedPoint()] - 1.5 + 0.5f), 3, 3);
            }

            if ( !kVOI.isSplit() )
            {
                drawTickMarks( kVOI, g, currentColor, unitsOfMeasure, m_kDrawingContext.getWidth(), m_kDrawingContext.getHeight(), resols);
            }
        }
    }


    private void drawVOIPoint( VOIBase kVOI, float[] resols, int[] unitsOfMeasure, Graphics g  ) {
        int iContourID = kVOI.getContourID();
        if ( iContourID != -1 )
        {
            iContourID++;
            kVOI.setLabel( String.valueOf(iContourID) );
        }
        drawVOIPoint( kVOI, g, kVOI.getLabel() );
    }

    private void drawVOIPoint( VOIBase kVOI, Graphics g, String label )
    {
        boolean doName = (Preferences.is(Preferences.PREF_SHOW_VOI_NAME) && kVOI.getName() != null);

        Vector3f kFile = kVOI.get(0);
        Vector3f kScreen = m_kDrawingContext.fileToScreen( kFile );
        int x = Math.round( kFile.X );
        int y = Math.round( kFile.Y );
        int xS = Math.round( kScreen.X );
        int yS = Math.round( kScreen.Y );

        String str;
        if (  kVOI.getType() != VOI.POLYLINE_SLICE )
        {
            str = new String("(" + x + "," + y + ")");
            //Create the string that will be drawn for label and name
            if (doName) {
                str = new String(kVOI.getName() + ": (" + x + "," + y + ")");
            } else if (label != null) {
                str = new String(label + ": (" + x + "," + y + ")");
            }

            int type = 0;
            String typeStr = Preferences.getProperty(Preferences.PREF_VOI_POINT_DRAW_TYPE);
            if (typeStr != null) {
                try {
                    type = Integer.parseInt(typeStr);
                    if (type < 0 || type > 3) {
                        type = 0;
                    }
                } catch (Exception ex) {}
            }

            switch (type) {
            case 0:
                g.drawLine(xS, yS - 4, xS, yS + 4);
                g.drawLine(xS - 4, yS, xS + 4, yS);
                break;
            case 1:
                g.drawLine(xS, yS - 4, xS, yS - 1);
                g.drawLine(xS, yS + 1, xS, yS + 4);
                g.drawLine(xS - 4, yS, xS - 1, yS);
                g.drawLine(xS + 1, yS, xS + 4, yS);
                break;
            case 2:
                g.drawLine(xS - 4, yS - 4, xS + 4, yS + 4);
                g.drawLine(xS - 4, yS + 4, xS + 4, yS - 4);
                break;
            case 3:
                g.drawLine(xS - 4, yS - 4, xS - 1, yS - 1);
                g.drawLine(xS + 1, yS + 1, xS + 4, yS + 4);
                g.drawLine(xS - 4, yS + 4, xS - 1, yS + 1);
                g.drawLine(xS + 1, yS - 1, xS + 4, yS - 4);
                break;
            }

            if ( !kVOI.isActive() ) {
                if (doName) {
                    if (xS < 20) {
                        g.drawString(kVOI.getName(), xS + 10, yS);
                    } else {
                        g.drawString(kVOI.getName(), xS - 15, yS - 5);
                    }
                } else {
                    if (xS < 20) {
                        g.drawString(label, xS + 10, yS);
                    } else {
                        g.drawString(label, xS - 15, yS - 5);
                    }
                }
            } else {

                if (type != 1 && type != 3) {
                    g.setColor(Color.black);
                    g.fillRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                    g.setColor(Color.white);
                    g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                }

                g.setFont(MipavUtil.font12);
                g.setColor(Color.yellow);

                int xPos = xS;
                int yPos = yS;
                if (xS < 70) {
                    xPos += 10;
                } else {
                    xPos -= 60;
                }
                if (yS < 30) {
                    yPos += 20;
                } else {
                    yPos -= 10;
                }
                g.drawString(str, xPos, yPos);
            }
        }
        else
        {
            VOIPoint kPolyPoint = (VOIPoint)kVOI;
            if (kPolyPoint.isActivePoint()) {
                str = new String(label + ": (" + x + "," + y + ")");
            } else {
                str = new String(label);
            }

            if (kPolyPoint.isActive()) {
                if (kPolyPoint.isActivePoint()) {
                    g.setColor(Color.GREEN);
                    g.fillRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                    g.setColor(Color.white);
                    g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                } else {
                    g.setColor(Color.black);
                    g.fillRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                    g.setColor(Color.white);
                    g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                }
            } else {
                g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                g.setColor(Color.white);
                g.fillRect((xS), (yS), 1, 1);
            }

            if (kPolyPoint.isActive()) {
                g.setFont(MipavUtil.font12);
                g.setColor(Color.yellow);

                int xPos = xS;
                int yPos = yS;

                boolean displaySegmentDistance = false;
                if (kPolyPoint.distanceString() != null) {
                    displaySegmentDistance = true;
                    displaySegmentDistance = !(kPolyPoint.distanceString().startsWith("0.00"));
                }

                if (kPolyPoint.isFirstSlicePoint() && kPolyPoint.isActivePoint()) {
                    if (xS < 20) {
                        g.drawString(label, xPos + 10, yPos - 5);
                        g.drawString("total: " + kPolyPoint.totalDistanceString(), xPos + 10, yPos - 18);
                        if (displaySegmentDistance)
                            g.drawString("segment: " + kPolyPoint.distanceString(), xPos + 10, yPos - 31);


                    } else {
                        g.drawString(label, xPos - 15, yPos - 5);
                        g.drawString("total: " + kPolyPoint.totalDistanceString(), xPos - 15, yPos - 18);
                        if (displaySegmentDistance)
                            g.drawString("segment: " + kPolyPoint.distanceString(), xPos - 15, yPos - 31);

                    }
                } else if (kPolyPoint.isFirstSlicePoint()) {
                    if (xS < 20) {
                        g.drawString(label, xPos + 10, yPos - 5);
                        g.drawString("total: " + kPolyPoint.totalDistanceString(), xPos + 10, yPos - 18);
                    } else {
                        g.drawString(label, xPos - 15, yPos - 5);
                        g.drawString("total: " + kPolyPoint.totalDistanceString(), xPos - 15, yPos - 18);
                    }
                } else if (kPolyPoint.isActivePoint()) {
                    if (xS < 20) {
                        g.drawString(label, xPos + 10, yPos - 5);

                        if (displaySegmentDistance)
                            g.drawString("segment: " + kPolyPoint.distanceString(), xPos + 10, yPos - 18);
                    }
                    else {
                        g.drawString(label, xPos - 15, yPos - 5);
                        if (displaySegmentDistance)
                            g.drawString("segment: " + kPolyPoint.distanceString(), xPos - 15, yPos - 18);
                    }
                } else {
                    if (xS < 20) {
                        g.drawString(label, xPos + 10, yPos - 5);
                    } else {
                        g.drawString(label, xPos - 15, yPos - 5);
                    }
                }
            } else {
                g.setFont(MipavUtil.font12);
                if (xS < 20) {
                    g.drawString(label, xS + 10, yS);
                } else {
                    g.drawString(label, xS - 15, yS - 5);
                }

            }

        }
    }






    private void drawVOIPolyLineSlice( VOIPolyLineSlice kVOI, float[] resols, int[] unitsOfMeasure, Graphics g, int slice, int orientation ) {
        Color color = g.getColor();

        String totalDistance = kVOI.getTotalLengthString(resols, unitsOfMeasure);
        String dist = new String();
        for ( int i = 0; i < kVOI.size(); i++ )
        {
            dist = kVOI.getLengthString( i, i+1, resols, unitsOfMeasure );
            kVOI.getPoints().get(i).setFirstPoint( i==0, i==kVOI.getSelectedPoint(), totalDistance, dist, i+1);
            int sliceI = getSlice( kVOI.getPoints().get(i) );
            if ( sliceI == m_iSlice )
            {
                drawVOIPoint( kVOI.getPoints().get(i), g, new Integer(i+1).toString() );
            }
        }
        for ( int i = 0; i < kVOI.size() - 1; i++ )
        {
            int sliceI = getSlice( kVOI.getPoints().get(i) );
            int sliceIP1 = getSlice( kVOI.getPoints().get(i+1) );
            if ( sliceI == sliceIP1 && sliceI == m_iSlice )
            {
                Vector3f kStart = m_kDrawingContext.fileToScreen( kVOI.get(i) );
                Vector3f kEnd = m_kDrawingContext.fileToScreen( kVOI.get(i+1) );
                g.setColor(color);
                g.drawLine((int) kStart.X, (int)kStart.Y, (int)kEnd.X, (int)kEnd.Y);
            }
        }
    }

    private void drawVOIProtractor( VOIBase kVOI, float[] resols, int[] unitsOfMeasure, Graphics g ) {

        Vector3f kStart = m_kDrawingContext.fileToScreen( kVOI.get(0) );
        Vector3f kMiddle = m_kDrawingContext.fileToScreen( kVOI.get(1) );
        Vector3f kEnd = m_kDrawingContext.fileToScreen( kVOI.get(2) );
        //0 is middle, 1 is start, 2 is end:
        float[] x = new float[3];
        x[0] = kMiddle.X;
        x[1] = kStart.X;
        x[2] = kEnd.X;

        //0 is middle, 1 is start, 2 is end:
        float[] y = new float[3];
        y[0] = kMiddle.Y;
        y[1] = kStart.Y;
        y[2] = kEnd.Y;

        if (Math.abs(y[1] - y[0]) > Math.abs(x[1] - x[0])) {
            g.drawLine((int) (x[0] - 1), (int) y[0], (int) (x[1] - 1), (int) y[1]);
            g.drawLine((int) x[0], (int) y[0], (int) x[1], (int) y[1]);
            g.drawLine((int) (x[0] + 1), (int) y[0], (int) (x[1] + 1), (int) y[1]);
        } else {
            g.drawLine((int) x[0], (int) (y[0] - 1), (int) x[1], (int) (y[1] - 1));
            g.drawLine((int) x[0], (int) y[0], (int) x[1], (int) y[1]);
            g.drawLine((int) x[0], (int) (y[0] + 1), (int) x[1], (int) (y[1] + 1));
        }

        g.drawLine((int) x[0], (int) y[0], (int) x[2], (int) y[2]);

        if (kVOI.isActive()) {
            this.drawTickMarks( kVOI, g, unitsOfMeasure, m_kDrawingContext.getWidth(), m_kDrawingContext.getHeight(), resols);
        } 
    }



    private void drawVOIText( VOIText kVOI, Graphics g) {

        Vector3f kScreen = m_kDrawingContext.fileToScreen( kVOI.get(0) );
        int xS = Math.round(kScreen.X);
        int yS = Math.round(kScreen.Y);

        kScreen = m_kDrawingContext.fileToScreen( kVOI.get(1) );
        int xS2 = Math.round(kScreen.X);
        int yS2 = Math.round(kScreen.Y);

        // draw the arrow if useMarker is true
        if ( kVOI.useMarker() ) {
            // determine the width/height of the TEXT (for marker line location)
            int width = (g.getFontMetrics(kVOI.getTextFont()).stringWidth( kVOI.getText()));
            int ascentValue = (int) (g.getFontMetrics(kVOI.getTextFont()).getStringBounds(kVOI.getText(), g).getHeight() / 2);

            int markerX = xS;
            int markerY = yS;

            if (xS2 > (xS + width)) {
                markerX = xS + width;
            } else if (xS2 <= xS) {
                markerX = xS - 2;
            } else {
                markerX = xS + width/2;
            }

            if (yS2 > yS) {
                markerY = yS + 3;
            } else if (yS2 <= (yS - ascentValue)) {
                markerY = yS - ascentValue - 5;
            } else {
                markerY = yS - ascentValue/2;
            }

            this.drawArrow( kVOI, (Graphics2D)g, markerX, markerY, xS2, yS2, .1f);
        } //arrow not off
        if ((kVOI.getTextFont() != null) && (kVOI.getTextFont().getName() == kVOI.getFontName()) && (kVOI.getTextFont().getStyle() == kVOI.getFontDescriptors())) {
            kVOI.setTextFont( kVOI.getTextFont().deriveFont(kVOI.getFontSize()) );

        } else {
            kVOI.setTextFont( new Font(kVOI.getFontName(), kVOI.getFontDescriptors(), kVOI.getFontSize()) );
        }

        Font previousFont = g.getFont();

        g.setFont(kVOI.getTextFont());

        if (kVOI.isActive()) {
            g.setColor(Color.RED);            
            g.drawString( kVOI.getText(), xS, yS + 1);
            g.drawString(kVOI.getText(), xS + 1, yS);
        } else {
            g.setColor( kVOI.getBackgroundColor() );
            g.drawString(kVOI.getText(), xS + 1, yS);
            g.drawString(kVOI.getText(), xS - 1, yS);
            g.drawString(kVOI.getText(), xS, yS - 1);
            g.drawString(kVOI.getText(), xS, yS + 1);
        }


        g.setColor( kVOI.getColor() );
        g.drawString(kVOI.getText(), xS, yS);
        g.setFont(previousFont);

    }




    private void getCoordsLine(float[] linePtsX, float[] linePtsY, double fraction, float[] coords) {
        float x1, y1;
        double vector1, vector2, tmp;
        double length;
        x1 = (linePtsX[0] + linePtsX[1]) / 2;
        y1 = (linePtsY[0] + linePtsY[1]) / 2;

        if (fraction == .25) {
            x1 = (linePtsX[0] + x1) / 2;
            y1 = (linePtsY[0] + y1) / 2;
        } else if (fraction == .75) {
            x1 = (x1 + linePtsX[1]) / 2;
            y1 = (y1 + linePtsY[1]) / 2;
        }

        vector1 = (linePtsX[1] - linePtsX[0]);
        vector2 = (linePtsY[1] - linePtsY[0]);
        length = Math.sqrt((vector1 * vector1) + (vector2 * vector2));
        vector1 = (linePtsX[1] - linePtsX[0]) / length;
        vector2 = (linePtsY[1] - linePtsY[0]) / length;
        tmp = vector1;
        vector1 = 5 * (-vector2);
        vector2 = 5 * tmp;
        coords[0] = (int) (x1 + vector1 + 0.5);
        coords[1] = (int) (y1 + vector2 + 0.5);
        coords[2] = (int) (x1 - vector1 + 0.5);
        coords[3] = (int) (y1 - vector2 + 0.5);
    }

    private void getCoordsProtractor(float[] x, float[] y, double fraction, float[] coords) {
        float x1, y1;
        double vector1, vector2, tmp;
        double length;
        x1 = (x[0] + x[1]) / 2;
        y1 = (y[0] + y[1]) / 2;

        if (fraction == .25) {
            x1 = (x[0] + x1) / 2;
            y1 = (y[0] + y1) / 2;
        } else if (fraction == .75) {
            x1 = (x1 + x[1]) / 2;
            y1 = (y1 + y[1]) / 2;
        }

        vector1 = (x[1] - x[0]);
        vector2 = (y[1] - y[0]);
        length = Math.sqrt((vector1 * vector1) + (vector2 * vector2));
        vector1 = (x[1] - x[0]) / length;
        vector2 = (y[1] - y[0]) / length;
        tmp = vector1;
        vector1 = 5 * (-vector2);
        vector2 = 5 * tmp;
        coords[0] = (int) (x1 + vector1 + 0.5);
        coords[1] = (int) (y1 + vector2 + 0.5);
        coords[2] = (int) (x1 - vector1 + 0.5);
        coords[3] = (int) (y1 - vector2 + 0.5);
    }







    private void getEndLinesLine(float[] linePtsX, float[] linePtsY, int line, float[] coords) {
        double vector1, vector2, tmp;
        double length;
        vector1 = (linePtsX[1] - linePtsX[0]);
        vector2 = (linePtsY[1] - linePtsY[0]);
        length = Math.sqrt((vector1 * vector1) + (vector2 * vector2));
        vector1 = (linePtsX[1] - linePtsX[0]) / length;
        vector2 = (linePtsY[1] - linePtsY[0]) / length;
        tmp = vector1;
        vector1 = 10 * ((vector1 * 0.707) + (vector2 * 0.707));
        vector2 = 10 * ((-tmp * 0.707) + (vector2 * 0.707));

        if (line == 0) {
            coords[0] = (int) (linePtsX[1]);
            coords[1] = (int) (linePtsY[1]);
            coords[2] = (int) (linePtsX[1] + vector1 + 0.5);
            coords[3] = (int) (linePtsY[1] + vector2 + 0.5);
        } else if (line == 1) {
            coords[0] = (int) (linePtsX[1]);
            coords[1] = (int) (linePtsY[1]);
            coords[2] = (int) (linePtsX[1] - vector2 + 0.5);
            coords[3] = (int) (linePtsY[1] + vector1 + 0.5);
        } else if (line == 2) {
            coords[0] = (int) (linePtsX[0]);
            coords[1] = (int) (linePtsY[0]);
            coords[2] = (int) (linePtsX[0] - vector1 + 0.5);
            coords[3] = (int) (linePtsY[0] - vector2 + 0.5);
        } else if (line == 3) {
            coords[0] = (int) (linePtsX[0]);
            coords[1] = (int) (linePtsY[0]);
            coords[2] = (int) (linePtsX[0] + vector2 + 0.5);
            coords[3] = (int) (linePtsY[0] - vector1 + 0.5);
        }
    }

    private void getEndLinesProtractor(float[] x, float[] y, int line, float[] coords) {
        double vector1, vector2, tmp;
        double length;
        vector1 = (x[1] - x[0]);
        vector2 = (y[1] - y[0]);
        length = Math.sqrt((vector1 * vector1) + (vector2 * vector2));
        vector1 = (x[1] - x[0]) / length;
        vector2 = (y[1] - y[0]) / length;
        tmp = vector1;
        vector1 = -10 * ((vector1 * 0.707) + (vector2 * 0.707));
        vector2 = 10 * ((tmp * 0.707) - (vector2 * 0.707));

        if (line == 0) {
            coords[0] = (int) (x[1]);
            coords[1] = (int) (y[1]);
            coords[2] = (int) (x[1] + vector1 + 0.5);
            coords[3] = (int) (y[1] + vector2 + 0.5);
        } else if (line == 1) {
            coords[0] = (int) (x[1]);
            coords[1] = (int) (y[1]);
            coords[2] = (int) (x[1] - vector2 + 0.5);
            coords[3] = (int) (y[1] + vector1 + 0.5);
        }
    }




    private int getSlice( VOIBase kVOI )
    {
        if ( kVOI.getType() == VOI.PROTRACTOR && ((VOIProtractor)kVOI).getAllSlices() )
        {
            return m_iSlice;
        }
        return (int)fileCoordinatesToPatient( kVOI.get(0) ).Z;
    }



    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    private void handleIntensityLineBtn3(MouseEvent mouseEvent) {

        // build VOI intensity popup menu
        JPopupMenu popupMenu = new JPopupMenu();
        JMenuItem menuItem = new JMenuItem("Show intensity graph");
        popupMenu.add(menuItem);
        menuItem.addActionListener(this);
        menuItem.setActionCommand(SHOW_INTENSITY_GRAPH);
        menuItem = new JMenuItem("Delete this intensity line");
        popupMenu.add(menuItem);
        menuItem.addActionListener(this);
        menuItem.setActionCommand(DELETE_INTENSITY_LINE);
        popupMenu.show(m_kComponent, mouseEvent.getX(), mouseEvent.getY());
    }





    private void initLiveWire( int iSlice, boolean bLiveWire )
    {
        if ( !m_bLiveWireInit )
        {
            if ( m_iPlaneOrientation != m_kImageActive.getImageOrientation() && 
                    m_iPlaneOrientation != FileInfoBase.UNKNOWN_ORIENT )
            {
                int[] aiAxisOrder = MipavCoordinateSystems.getAxisOrder(m_kImageActive, m_iPlaneOrientation);
                boolean[] abAxisFlip = MipavCoordinateSystems.getAxisFlip(m_kImageActive, m_iPlaneOrientation);
                m_kLocalImage = m_kImageActive.export( aiAxisOrder, abAxisFlip );
            }
            else
            {
                m_kLocalImage = m_kImageActive;
            }

            m_bLiveWireInit = true;
            int nSlices = m_aiLocalImageExtents.length > 2 ? m_aiLocalImageExtents[2] : 1;
            m_abInitLiveWire = new boolean[ nSlices ];
            for ( int i = 0; i < m_abInitLiveWire.length; i++ )
            {
                m_abInitLiveWire[i] = false;
            }
        }
        if ( m_abInitLiveWire[iSlice] )
        {
            return;
        }

        int xDim = m_kLocalImage.getExtents()[0];
        int yDim = m_kLocalImage.getExtents()[1];

        int length = xDim * yDim;
        imageBufferActive = null;

        // for color images, arrays need to be 4 times bigger
        if (m_kLocalImage.isColorImage()) {

            // direction of the unit vector of the partial derivative in the x direction
            xDirections = new float[length * 4];

            // direction of the unit vector of the partial derivative in the y direction
            yDirections = new float[length * 4];

            imageBufferActive = new float[length * 4];
        } else {

            // direction of the unit vector of the partial derivative in the x direction
            xDirections = new float[length];

            // direction of the unit vector of the partial derivative in the y direction
            yDirections = new float[length];

            imageBufferActive = new float[length];
        }

        try {
            m_kLocalImage.exportData(iSlice * imageBufferActive.length, imageBufferActive.length, imageBufferActive);
        } catch (IOException error) {
            MipavUtil.displayError("Error while trying to retrieve RGB data.");
        }

        if ( bLiveWire )
        {
            ViewJProgressBar progressBar = new ViewJProgressBar(m_kImageActive.getImageName(),
                    "Livewire: Computing cost function ...", 0, 100, false, this, null);
            localCosts = RubberbandLivewire.getLocalCosts( m_kLocalImage, m_iLiveWireSelection, 
                    imageBufferActive,
                    xDirections, yDirections, progressBar );

            costGraph = new byte[localCosts.length]; // Graph with arrows from each node to next one

            // A node is a location i in the array; where it
            // points to is the value costGraph[i].
            processedIndicies = new BitSet(localCosts.length); // Boolean indicating if pixel at location
            // has been processed.
            seededCosts = new float[localCosts.length]; // Seeded costs, so looking up a cost that has been
            // set is easy
            activeTree = new ActiveTree(); // List of active nodes to expand; reset on seed(pt) call


            m_abInitLiveWire[iSlice] = true;
            if (progressBar != null) {
                progressBar.dispose();
            }
        }
    }

    private void moveVOIPoint( int iX, int iY )
    {
        if ( m_kCurrentVOI == null )
        {            
            return;
        }
        setPosition( m_kCurrentVOI, m_kCurrentVOI.getNearPoint(), iX, iY, m_iSlice ); 
        m_kParent.setCursor(MipavUtil.crosshairCursor); 
        m_kParent.updateDisplay();
        if ( m_kCurrentVOI.getGroup().getContourGraph() != null )
        {
            m_kParent.updateGraph(m_kCurrentVOI);
        }
    }



    private void pasteVOI( int iSlice )
    {
        if ( m_kCopyVOI == null )
        {
            return;
        }
        m_kCurrentVOI = m_kCopyVOI.clone();
        m_kCurrentVOI.clear();
        for ( int i = 0; i < m_kCopyVOI.size(); i++ )
        {
            Vector3f kPos = fileCoordinatesToPatient( m_kCopyVOI.get(i) );
            kPos.Z = iSlice;
            m_kCurrentVOI.add( patientCoordinatesToFile( kPos ) );
        }


        m_kParent.pasteVOI(m_kCurrentVOI);
        m_kCurrentVOI.setActive(false);
    }




    /**
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    private void processLeftMouseDrag(MouseEvent kEvent) {

        if ( m_bDrawVOI && !(((m_iDrawType == POINT) || (m_iDrawType == POLYPOINT) ||
                (m_iDrawType == LIVEWIRE) || (m_iDrawType == LEVELSET) || (m_iDrawType == TEXT))) )
        {
            createVOI( kEvent.getX(), kEvent.getY() );
        } 
        else if ( m_bPointer )
        {
            if ( m_iNearStatus == NearPoint )
            {
                if ( m_bFirstDrag && ((m_fMouseX != kEvent.getX()) || (m_fMouseY != kEvent.getY())) )
                {
                    m_kParent.saveVOIs( "movePoint" );
                    m_bFirstDrag = false;
                }
                moveVOIPoint( kEvent.getX(), kEvent.getY() );
            }
            else if ( m_bSelected )
            {
                if ( m_bFirstDrag && ((m_fMouseX != kEvent.getX()) || (m_fMouseY != kEvent.getY())) )
                {
                    m_kParent.saveVOIs( "moveVOI" );
                    m_bFirstDrag = false;
                }    
                m_kParent.moveVOI( this, new Vector3f( kEvent.getX() - m_fMouseX, kEvent.getY() - m_fMouseY, 0 ), m_iPlane, m_bFirstDrag );
                m_fMouseX = kEvent.getX();
                m_fMouseY = kEvent.getY();
            }
        }
    }

    private Polygon scalePolygon( VOIBase kVOI ) {
        int i;
        int x;
        int y;
        Polygon scaledGon = null;

        try {
            scaledGon = new Polygon();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        for (i = 0; i < kVOI.size(); i++) {
            Vector3f kVolumePt = kVOI.elementAt(i);
            Vector3f kScreen = m_kDrawingContext.fileToScreen( kVolumePt );

            x = (int) kScreen.X;
            y = (int) kScreen.Y;
            scaledGon.addPoint(x, y);
        }

        return scaledGon;
    }



    /**
     * Sets up directed graph from the seed point. A point (x,y) is mapped to its absolute value in the image, y*xDim +
     * x. This along with its cost is stored in Node. The structure costGraph holds the "edges" of the graph. Each
     * location has an integer associated with it which represents where that node is pointing to. Thus costGraph[7] = 8
     * would mean the node at 7 (really (0,7)) is pointing to position 8 (really (0,8)). The only possibilities for a
     * location in costGraph are the 8 neighbors surrounding that node. For this reason we might use a byte array
     * instead of an integer array. The seed point points nowhere, to indicate that it's the seed;
     * costGraph[seed.location] = -1. We also need to know if a point has been processed, that is, expanded with the
     * cost set. For this we use a BitSet whose size is the same as the number of pixels in the image. Once a point is
     * processed, its location in the BitSet is set to <code>true</code>.
     *
     * <p>The array seededCosts holds the costs so far for a location. If a cost has not been assigned yet, the cost is
     * -2. ActiveList is simply a linked list of Integers; the Integer refers to the location in the seededCosts array.
     * ActiveList is sorted by cost, so that the minimum cost in the ActiveList is the first element of the linked list.
     * Thus finding the minimum is O(1). Finding out if an element is in the ActiveList is also O(1), because for an
     * element to be in the list, it must have already been assigned a cost. Therefore, if seededCosts[location] != -2,
     * it is in the ActiveList. And finding the cost of an item in the ActiveList is O(1), because it's just
     * seededCosts[location], where location is the Integer in the ActiveList. Obviously we're winning speed at the
     * expense of memory, but it's not too much memory in the overall scheme of things.</p>
     *
     * <p>The gradient direction component of the cost is added in on the fly. This is because to precalculate would
     * mean an array of 8n, where n is the size of the image. The link from p to q is not the same as the link from q to
     * p. Furthermore, it may never be necessary to calculate some of the links, because the graph would never look at
     * that pair. For more information on how the gradient direction cost is calculated, look at the comments directly
     * above the code.</p>
     *
     * @param  pt  Point to seed with.
     */
    private void seed(int iX, int iY) {
        Vector3f kVolumePt = new Vector3f();
        m_kDrawingContext.screenToFile( iX, iY, m_iSlice, kVolumePt );
        Vector3f kLocalPt = fileCoordinatesToPatient( kVolumePt );

        int xDim = m_aiLocalImageExtents[0];
        int yDim = m_aiLocalImageExtents[1];
        int x = (int)kLocalPt.X;
        int y = (int)kLocalPt.Y;

        int location = (y * xDim) + x; // Location in array that represents image.

        costGraph[location] = -1; // No parent of seed point
        seedPoint = location;

        // processedIndicies.clear(); only in JVM 1.4.
        for (int c = 0; c < processedIndicies.size(); c++) {
            processedIndicies.clear(c);
        }


        for (int i = 0; i < seededCosts.length; i++) {
            seededCosts[i] = -2;
        }

        activeTree.reset();

        seededCosts[location] = 0;
        activeTree.insert(location, 0f);

        float temp, cost, gradDir;
        System.currentTimeMillis();
        int count = 0;

        float gdConst = (float) (2f / (3 * Math.PI));
        float pi = (float) Math.PI;

        while (activeTree.isEmpty() == false) { // while active list has more elements to process

            location = activeTree.pop();
            cost = seededCosts[location];
            processedIndicies.set(location); // set this point as processed
            x = location % xDim;
            y = location / xDim;

            count++;

            for (int iy = -1; iy <= 1; iy++) {
                int yOffset = (y + iy) * xDim;

                if (((y + iy) >= 0) && ((y + iy) < yDim)) { // in bounds in the y dimension

                    for (int ix = -1; ix <= 1; ix++) {
                        int position = yOffset + (x + ix);

                        if (((x + ix) >= 0) && ((x + ix) < xDim) && // in bounds in the x dimension
                                !processedIndicies.get(position)) { // not yet processed - this will rule out current
                            // node

                            if (m_iLiveWireSelection == RubberbandLivewire.GRADIENT_MAG) {
                                // Gradient Direction Cost Let D(p) be a unit vector of the gradient direction at point
                                // p. Define D'(p) as the unit vector perpendicular to D(p). In our implementation, D(p)
                                // = (xDirections[location], yDirections[location]) where location is the absolute
                                // position of point p; that is, location = p.y*xDim + p.x So       D(p) =
                                // (xDirections[location], yDirections[location])       D'(p) = (yDirections[location],
                                // -xDirections[location])
                                //
                                // The formulation of the gradient direction feature cost is      f(p,q) = 2/(3pi) *
                                // (acos(dp(p,q) + acos(dq(p,q)))) where      dp(p,q) = D'(p) dot L(p,q)      dq(p,q) =
                                // L(p,q) dot D'(q) and
                                //
                                //     L(p,q) =    1     {q - p if D'(p) dot (q - p) >= 0               ------- *{
                                //       ||p-q||  {p - q if D'(p) dot (q - p) < 0

                                float Lx, Ly;
                                float divide;

                                // divide is || p - q || = || ( (x - (x+ix)), (y - (y+iy)) ) ||
                                // = || ( -ix, -iy ) ||
                                // = sqrt(ix^2 + iy^2)
                                // = sqrt(2) or sqrt(1)
                                // because ix and iy will never both be 0 (current node is already processed)
                                // so if they are both non-zero, it's sqrt(2), otherwise it's sqrt(1)
                                if ((ix != 0) && (iy != 0)) {
                                    divide = 0.7071068f;
                                } else {
                                    divide = 1f;
                                }

                                // if D'(p) dot (q - p) >= 0
                                // becomes
                                // if ( yDirections[location]*(x+ix-x) + (-xDirections[location]*(y+iy-y) )
                                // becomes
                                if (((yDirections[location] * ix) - (xDirections[location] * iy)) >= 0) {
                                    Lx = ix * divide;
                                    Ly = iy * divide;
                                }
                                // D'(p) dot (q - p) < 0
                                else {
                                    Lx = -ix * divide;
                                    Ly = -iy * divide;
                                }
                                // if (yDirections[location]*ix - xDirections[location]*iy >= 0)
                                // dp = divide * (yDirections[location]*ix - xDirections[location]*iy)
                                // so dp >= 0
                                // if (yDirections[location]*ix - xDirections[location]*iy < 0)
                                // dp = -divide * (yDirections[location]*ix - xDirectios[locations]*iy)
                                // so dp > 0
                                // Hence always have 1 >= dp >= 0
                                // dq = sign(yDirections[location]*ix - xDirections[location]*iy) *
                                // divide * (yDirections[position]*ix - xDirections[position]*iy)
                                // Thus, if (yDirections[location]*ix - xDirections[location]*iy) and
                                // (yDirections[position]*ix - xDirections[position]*iy) have the same
                                // sign, then dq is positive.  Otherwise, dq is negative.
                                // 1 > = dq > = -1
                                // acos_dp can vary from 0 to pi/2 and acos_dq can vary from 0 to
                                // pi so the gradient direction feature cost can vary from 0 to 1.
                                // The Taylor series for acos(x) =
                                // pi/2 - x - (x**3)/(2*3) - 1*3*(x**5)/(2*4*5) - 1*3*5*(x**7)/(2*4*6*7)

                                float dp = (yDirections[location] * Lx) - (xDirections[location] * Ly);
                                float dq = (yDirections[position] * Lx) - (xDirections[position] * Ly);

                                if (dp > 1) {
                                    dp = 1f;
                                }

                                if (dq > 1) {
                                    dq = 1f;
                                } else if (dq < -1) {
                                    dq = -1f;
                                }

                                // float acos_dp = (float)(Math.acos(dp));
                                // float acos_dq = (float)(Math.acos(dq));
                                // float gradDir = (gdConst)*(acos_dp + acos_dq);

                                // The above is the original formula as described.
                                // The below is what we're using because it's much faster since
                                // it only uses up to the third power term in the Taylor series.
                                gradDir = gdConst * (pi - dp - (dp * dp * dp / 6) - dq - (dq * dq * dq / 6));
                            } else { // ((selection == MEDIALNESS) || (selection == INTENSITY))
                                gradDir = 0f;
                            }

                            if ((ix != 0) && (iy != 0)) { // diagonal costs more
                                temp = cost + ((localCosts[position] + (grad_weight * gradDir)) * 1.4142f); // * square root of 2, Euclidean distance
                            } else {
                                temp = cost + (localCosts[position] + (grad_weight * gradDir)); // temp cost
                            }

                            if (temp < seededCosts[position]) { // not set seededCosts == -2 which will always be less
                                // than temp, temp must be positive
                                activeTree.remove(position, seededCosts[position]);
                                seededCosts[position] = -2;
                            }

                            // if not in active or was just removed from active list
                            if (seededCosts[position] == -2) {

                                // put into active list or back into active list with new cost
                                // set pointer back to parent
                                int temploc = ((-iy + 1) * 3) - ix + 1;

                                costGraph[position] = (byte) temploc;
                                activeTree.insert(position, temp);
                                seededCosts[position] = temp;

                            }
                        }
                    }
                }
            }
        }


    }

    private VOIBase selectVOI( int iX, int iY, boolean bShiftDown )
    {
        m_kParent.selectAllVOIs(false);
        m_bSelected = false;
        m_kCurrentVOI = null;
        VOIVector kVOIs = m_kImageActive.getVOIs();
        for ( int i = kVOIs.size()-1; i >=0; i-- )
        {
            VOI kVOI = kVOIs.get(i);
            for ( int j = kVOI.getCurves().size()-1; j >= 0; j-- )
            {
                VOIBase kVOI3D = kVOI.getCurves().get(j);
                if ( (m_iPlane == (m_iPlane & kVOI3D.getPlane())) &&
                        (m_iSlice == getSlice( kVOI3D )) && 
                        contains( kVOI3D, iX, iY, m_iSlice ) )
                {
                    m_kCurrentVOI = kVOI3D;
                    m_kParent.setSelectedVOI( m_kCurrentVOI.getGroup(), bShiftDown );
                    m_kCurrentVOI.setActive(true);
                    m_bSelected = true;
                    return m_kCurrentVOI;
                }
            }
        }
        return m_kCurrentVOI;
    }


    private void setPosition( VOIBase kVOI, int iPos, float fX, float fY, float fZ )
    {
        setPosition( kVOI, iPos, new Vector3f( fX, fY, fZ ) );
    }



    private void setPosition( VOIBase kVOI, int iPos, Vector3f kPos )
    {
        if ( kVOI.isFixed() )
        {
            return;
        }

        Vector3f kVolumePt = new Vector3f();
        if ( iPos < kVOI.size() )
        {
            m_kDrawingContext.screenToFile( (int)kPos.X, (int)kPos.Y, (int)kPos.Z, kVolumePt );
            kVOI.set( iPos, kVolumePt );
            kVOI.setSelectedPoint( iPos );
            kVOI.update();
        }
    }


    private void showSelectedVOI( int iX, int iY )
    {
        m_kComponent.removeMouseListener(m_kPopupPt);
        m_kComponent.removeMouseListener(m_kPopupVOI);
        Vector3f kVolumePt = new Vector3f();
        m_kDrawingContext.screenToFile( iX, iY, m_iSlice, kVolumePt );

        if ( m_kCurrentVOI != null && (m_iSlice == getSlice( m_kCurrentVOI )) )
        {
            if ( nearPoint( m_kCurrentVOI, iX, iY, m_iSlice ) )
            {
                if ( m_kCurrentVOI.getType() == VOI.POINT )
                {
                    m_kParent.setCursor(MipavUtil.moveCursor);
                    m_iNearStatus = NearNone;
                }
                else
                {
                    m_kParent.setCursor(MipavUtil.crosshairCursor);
                    m_iNearStatus = NearPoint;
                }
                return;
            }
            else if ( m_kCurrentVOI.nearLine( (int)kVolumePt.X, (int)kVolumePt.Y, (int)kVolumePt.Z ) )
            {
                if ( (m_kCurrentVOI.getType() == VOI.CONTOUR) ||
                        (m_kCurrentVOI.getType() == VOI.POLYLINE) )
                {
                    m_kParent.setCursor(MipavUtil.addPointCursor);
                    m_iNearStatus = NearLine;
                }
                else
                {
                    m_kParent.setCursor(MipavUtil.moveCursor);
                    m_iNearStatus = NearNone;
                }
                return;
            }
        }

        VOIVector kVOIs = m_kImageActive.getVOIs();
        for ( int i = kVOIs.size()-1; i >=0; i-- )
        {
            VOI kVOI = kVOIs.get(i);
            for ( int j = kVOI.getCurves().size()-1; j >= 0; j-- )
            {
                VOIBase kVOI3D = kVOI.getCurves().get(j);
                if ( (m_iPlane == (m_iPlane & kVOI3D.getPlane())) &&
                        (m_iSlice == getSlice( kVOI3D )) &&
                        contains( kVOI3D, iX, iY, m_iSlice ) )
                {
                    m_iNearStatus = NearNone;
                    m_kParent.setCursor(MipavUtil.moveCursor);
                    return;
                }
            }
        }
        m_iNearStatus = NearNone;
        m_kParent.setCursor(MipavUtil.defaultCursor);
        // not: m_kParent.setDefaultCursor() which changes the cursorMode...
    }


    private VOIBase split( VOIBase kVOI, Vector3f kStartPt, Vector3f kEndPt )
    {
        if ( kVOI.getType() != VOI.CONTOUR )
        {
            return null;
        }
        int iVOISlice = (int)fileCoordinatesToPatient(kVOI.get(0)).Z;
        int iPoints = kVOI.size();
        Vector3f kFirstIntersectionPt = null;
        Vector3f kSecondIntersectionPt = null;
        int iFirstIndex = -1;
        int iSecondIndex = -1;
        for ( int iP = 0; iP < (iPoints - 1) && (kSecondIntersectionPt == null); iP++ )
        {
            Vector3f kLocal1 = fileCoordinatesToPatient(kVOI.get(iP));
            Vector3f kLocal2 = fileCoordinatesToPatient(kVOI.get(iP+1));
            Vector3f kIntersection = new Vector3f();

            if (JDialogVOISplitter.intersects( kLocal1, kLocal2, kStartPt, kEndPt, kIntersection )) {
                if (kFirstIntersectionPt == null)
                {
                    kFirstIntersectionPt = kIntersection;
                    iFirstIndex = iP;
                } 
                else 
                {
                    kSecondIntersectionPt = kIntersection;
                    iSecondIndex = iP;
                }
            }
        }
        if ( kSecondIntersectionPt == null )
        {
            Vector3f kLocal1 = fileCoordinatesToPatient(kVOI.lastElement());
            Vector3f kLocal2 = fileCoordinatesToPatient(kVOI.firstElement());
            Vector3f kIntersection = new Vector3f();

            if (JDialogVOISplitter.intersects( kLocal1, kLocal2, kStartPt, kEndPt, kIntersection )) {

                kSecondIntersectionPt = kIntersection;
                iSecondIndex = iPoints - 1;
            }
        }

        if (kFirstIntersectionPt != null && kSecondIntersectionPt != null) 
        {        
            kFirstIntersectionPt.Z = iVOISlice;
            kFirstIntersectionPt = patientCoordinatesToFile(kFirstIntersectionPt);
            kSecondIntersectionPt.Z = iVOISlice;
            kSecondIntersectionPt = patientCoordinatesToFile(kSecondIntersectionPt);


            Vector<Vector3f> kPositions = new Vector<Vector3f>();
            kPositions.add( kSecondIntersectionPt );  
            //check if there are points from second index to 0-index, add those first
            for (int iP = iSecondIndex + 1; iP < iPoints; iP++) {
                kPositions.add(kVOI.get(iP));
            }
            for (int iP = 0; iP < iFirstIndex + 1; iP++) {
                kPositions.add(kVOI.get(iP));
            }
            kPositions.add( kFirstIntersectionPt );  

            for (int iP = 00; iP < kPositions.size(); iP++) {
                kVOI.remove(kPositions.get(iP));
            }
            kVOI.add(0, new Vector3f(kFirstIntersectionPt) );
            kVOI.add(0, new Vector3f(kSecondIntersectionPt) );      

            kVOI.setSelectedPoint(0);
            kVOI.update();

            return new VOIContour( kVOI.isFixed(), kVOI.isClosed(), kPositions );
        }
        return null;       
    }



    private void splitVOIs( boolean bAllSlices, boolean bOnlyActive, VOIBase kSplitVOI )
    {
        VOIVector kVOIs = m_kImageActive.getVOIs();
        if ( !kSplitVOI.isSplit() || (kVOIs.size() == 0) )
        {
            return;
        }
        Vector<VOIBase> kNewVOIs = new Vector<VOIBase>();

        Vector3f kStartPt = fileCoordinatesToPatient(kSplitVOI.get(0)); 
        Vector3f kEndPt = fileCoordinatesToPatient(kSplitVOI.get(1)); 

        int iStartSlice = bAllSlices? 0 : m_iSlice;
        int nSlices = m_aiLocalImageExtents.length > 2 ? m_aiLocalImageExtents[2] : 1;
        int iEndSlice = bAllSlices? nSlices : m_iSlice + 1;


        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kVOI = kVOIs.get(i);
            for ( int j = 0; j < kVOI.getCurves().size(); j++ )
            {
                VOIBase kVOI3D = kVOI.getCurves().get(j);
                if ( m_iPlane != (m_iPlane & kVOI3D.getPlane() ))
                {
                    continue;
                }
                int iVOISlice = kVOI3D.slice();
                if ( !((iVOISlice >= iStartSlice) && (iVOISlice < iEndSlice)) )
                {
                    continue;
                }
                if ( !(!bOnlyActive || kVOI3D.isActive()) )
                {
                    continue;
                }
                VOIBase kNew = split( kVOI3D, kStartPt, kEndPt );
                if ( kNew != null )
                {  
                    //m_kParent.updateCurrentVOI( kVOI, kVOI );
                    kNewVOIs.add(kNew);
                }
            }
        }
        m_kParent.deleteVOI( kSplitVOI );  
        kSplitVOI.dispose();
        kSplitVOI = null;

        for ( int i = 0; i < kNewVOIs.size(); i++ )
        {
            m_kParent.addVOI(kNewVOIs.get(i), true);
        }
        m_kCurrentVOI = null;
    }

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    /**
     * This method calculates the average pixel value based on the four neighbors (N, S, E, W).
     *
     * @param   index  the center pixel where the average pixel value is to be calculated.
     *
     * @return  the average pixel value as a float.
     */
    private float avgPix(int index) {
        int xDim = m_aiLocalImageExtents[0];

        if ((index > xDim) && (index < (imageBufferActive.length - xDim))) {

            float sum = imageBufferActive[index];

            sum += imageBufferActive[index - xDim];
            sum += imageBufferActive[index - 1];
            sum += imageBufferActive[index + 1];
            sum += imageBufferActive[index + xDim];

            return sum / 5.0f;
        } else {
            return (imageBufferActive[index]);
        }
    }

    /**
     * Generates the possible paths of the level set and pushes them onto a stack. Looks in the 8 neighborhood
     * directions for the possible paths.
     *
     * @param  index  image location
     * @param  i      DOCUMENT ME!
     */
    private void paths(int index, int i, float level) {

        int[] intPtr = null;

        try {
            intPtr = new int[1];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.mouseDragged");

            return;
        }

        int xDim = m_aiLocalImageExtents[0];
        intPtr[0] = levelSetStack.size() - 1;

        if ((i != 0) && (imageBufferActive[index - xDim - 1] <= level) && (map.get(index - xDim - 1) == false)) {
            stack.push(intPtr);
        } else if ((i != 1) && (imageBufferActive[index - xDim] <= level) && (map.get(index - xDim) == false)) {
            stack.push(intPtr);
        } else if ((i != 2) && (imageBufferActive[index - xDim + 1] <= level) && (map.get(index - xDim + 1) == false)) {
            stack.push(intPtr);
        } else if ((i != 3) && (imageBufferActive[index + 1] <= level) && (map.get(index + 1) == false)) {
            stack.push(intPtr);
        } else if ((i != 4) && (imageBufferActive[index + xDim + 1] <= level) && (map.get(index + xDim + 1) == false)) {
            stack.push(intPtr);
        } else if ((i != 5) && (imageBufferActive[index + xDim] <= level) && (map.get(index + xDim) == false)) {
            stack.push(intPtr);
        } else if ((i != 6) && (imageBufferActive[index + xDim - 1] <= level) && (map.get(index + xDim - 1) == false)) {
            stack.push(intPtr);
        } else if ((i != 7) && (imageBufferActive[index - 1] <= level) && (map.get(index - 1) == false)) {
            stack.push(intPtr);
        }
    }

    /**
     * Creates a single level set. Takes a starting point and finds a closed path along the levelset back to the
     * starting point.
     *
     * @param  startPtX  the start point
     * @param  startPtY  the start point
     * @param  level     the level of the level set
    private VOIBase singleLevelSet(float startPtX, float startPtY, float level) {

        int x, y;
        int index;
        double distance;
        stack.removeAllElements();

        if (imageBufferActive == null) {
            return;
        }

        int xDim = m_aiLocalImageExtents[0];
        int yDim = m_aiLocalImageExtents[1];

        for (int i = 0; i < map.size(); i++) {
            map.clear(i);
        }

        if (startPtX >= (xDim - 1)) {
            return;
        }

        if (startPtY >= (yDim - 1)) {
            return;
        }

        x = (int) (startPtX + 0.5);
        y = (int) (startPtY + 0.5);

        levelSetStack.reset();
        levelSetStack.addPoint(x, y);
        map.set((y * xDim) + x);

        int dir = -1;
        float diff = 100000;

        do {
            index = (y * xDim) + x;

            if ((x >= 2) && (x < (xDim - 2)) && (y >= 2) && (y < (yDim - 2))) {

                if ((avgPix(index - xDim) >= level) &&
                        ((avgPix(index - xDim + 1) < level) || (avgPix(index) < level) ||
                             (avgPix(index - xDim - 1) < level) || (avgPix(index - (2 * xDim)) < level)) &&
                        (map.get(index - xDim) == false)) {
                    dir = 1;
                    diff = Math.abs(avgPix(index - xDim) - avgPix(index));
                }

                if ((avgPix(index - xDim + 1) >= level) &&
                        ((avgPix(index - xDim + 2) < level) || (avgPix(index + 1) < level) ||
                             (avgPix(index - xDim) < level) || (avgPix(index - (2 * xDim) + 1) < level)) &&
                        (map.get(index - xDim + 1) == false)) {

                    if (Math.abs(avgPix(index - xDim + 1) - avgPix(index)) < diff) {
                        dir = 2;
                        diff = Math.abs(avgPix(index - xDim + 1) - avgPix(index));
                    }
                }

                if ((avgPix(index + 1) >= level) &&
                        ((avgPix(index + 2) < level) || (avgPix(index + xDim + 1) < level) || (avgPix(index) < level) ||
                             (avgPix(index - xDim + 1) < level)) && (map.get(index + 1) == false)) {

                    if (Math.abs(avgPix(index + 1) - avgPix(index)) < diff) {
                        dir = 3;
                        diff = Math.abs(avgPix(index + 1) - avgPix(index));
                    }
                }

                if ((avgPix(index + xDim + 1) >= level) &&
                        ((avgPix(index + xDim + 2) < level) || (avgPix(index + (2 * xDim) + 1) < level) ||
                             (avgPix(index + 1) < level) || (avgPix(index + xDim) < level)) &&
                        (map.get(index + xDim + 1) == false)) {

                    if (Math.abs(avgPix(index + xDim + 1) - avgPix(index)) < diff) {
                        dir = 4;
                        diff = Math.abs(avgPix(index + xDim + 1) - avgPix(index));
                    }
                }

                if ((avgPix(index + xDim) >= level) &&
                        ((avgPix(index + xDim + 1) < level) || (avgPix(index + (2 * xDim)) < level) ||
                             (avgPix(index + xDim - 1) < level) || (avgPix(index) < level)) &&
                        (map.get(index + xDim) == false)) {

                    if (Math.abs(avgPix(index + xDim) - avgPix(index)) < diff) {
                        dir = 5;
                        diff = Math.abs(avgPix(index + xDim) - avgPix(index));
                    }
                }

                if ((avgPix(index + xDim - 1) >= level) &&
                        ((avgPix(index + xDim) < level) || (avgPix(index + (2 * xDim) - 1) < level) ||
                             (avgPix(index + xDim - 2) < level) || (avgPix(index - 1) < level)) &&
                        (map.get(index + xDim - 1) == false)) {

                    if (Math.abs(avgPix(index + xDim - 1) - avgPix(index)) < diff) {
                        dir = 6;
                        diff = Math.abs(avgPix(index + xDim - 1) - avgPix(index));
                    }
                }

                if ((avgPix(index - 1) >= level) &&
                        ((avgPix(index) < level) || (avgPix(index + xDim - 1) < level) || (avgPix(index - 2) < level) ||
                             (avgPix(index - xDim - 1) < level)) && (map.get(index - 1) == false)) {

                    if (Math.abs(avgPix(index - 1) - avgPix(index)) < diff) {
                        dir = 7;
                        diff = Math.abs(avgPix(index - 1) - avgPix(index));
                    }
                }

                if ((avgPix(index - xDim - 1) >= level) &&
                        ((avgPix(index - xDim) < level) || (avgPix(index - 1) < level) ||
                             (avgPix(index - xDim - 2) < level) || (avgPix(index - (2 * xDim) - 1) < level)) &&
                        (map.get(index - xDim - 1) == false)) {

                    if (Math.abs(avgPix(index - xDim - 1) - avgPix(index)) < diff) {
                        dir = 0;
                        // diff = Math.abs(imageBufferActive[index-xDim-1] - imageBufferActive[index]);
                    }
                }

                diff = 1000000;

                if (dir == 1) {
                    // x = x;
                    y = y - 1;
                    map.set(index - xDim);
                    paths(index, 1, level);
                } else if (dir == 2) {
                    x = x + 1;
                    y = y - 1;
                    map.set(index - xDim + 1);
                    paths(index, 2, level);
                } else if (dir == 3) {
                    x = x + 1;
                    // y = y;
                    map.set(index + 1);
                    paths(index, 3, level);
                } else if (dir == 4) {
                    x = x + 1;
                    y = y + 1;
                    map.set(index + xDim + 1);
                    paths(index, 4, level);
                } else if (dir == 5) {
                    // x = x;
                    y = y + 1;
                    map.set(index + xDim);
                    paths(index, 5, level);
                } else if (dir == 6) {
                    x = x - 1;
                    y = y + 1;
                    map.set(index + xDim - 1);
                    paths(index, 6, level);
                } else if (dir == 7) {
                    x = x - 1;
                    // y = y;
                    map.set(index - 1);
                    paths(index, 7, level);
                } else if (dir == 0) {
                    x = x - 1;
                    y = y - 1;
                    map.set(index - xDim - 1);
                    paths(index, 0, level);
                } else {

                    if (!stack.empty()) {
                        int ptr = ((int[]) stack.pop())[0];
                        x = levelSetStack.getPointX(ptr);
                        y = levelSetStack.getPointY(ptr);
                        levelSetStack.setIndex(ptr);
                    } else {
                        x = y = -1;
                    }
                }

                dir = -1;
            } else { // near edge of image
                levelSetStack.reset();

                break;
            }

            if ((x == -1) || (y == -1)) {
                levelSetStack.reset();

                break;
            }

            levelSetStack.addPoint(x, y);

            distance = ((x - startPtX) * (x - startPtX)) + ((y - startPtY) * (y - startPtY));

            if ((distance < 2.1) && (levelSetStack.size() < 10)) {
                distance = 10;
            }
        } while (distance > 2.1);


        if (levelSetStack.size() != 0) {
            return levelSetStack.exportPolygon();
        } else {
            System.err.println( "singleLevelSet return null" );
            return null;
        }
    }
    
     */
    
    
    /**
     * Creates a single level set. Takes a starting point and finds a closed path along the levelset back to the
     * starting point.
     *
     * @param  startPtX  the start point
     * @param  startPtY  the start point
     * @param  level     the level of the level set
     */
    private VOIBase singleLevelSet2(float startPtX, float startPtY) {

        int x, y;
        int index;
        double distance;
        stack.removeAllElements();

        if (imageBufferActive == null) {
            return null;
        }

        int xDim = m_kLocalImage.getExtents()[0];
        int yDim = m_kLocalImage.getExtents()[1];

        for (int i = 0; i < map.size(); i++) {
            map.clear(i);
        }


        new Vector3f( startPtX, startPtY, m_iSlice );
        Vector3f kVolumePt = new Vector3f();
        m_kDrawingContext.screenToFile( (int)startPtX, (int)startPtY, m_iSlice, kVolumePt );
        Vector3f kPatientPt = new Vector3f();
        MipavCoordinateSystems.fileToPatient( kVolumePt, kPatientPt, m_kImageActive, m_iPlaneOrientation );

        startPtX = kPatientPt.X;
        startPtY = kPatientPt.Y;        

        if (startPtX >= (xDim - 1)) {
            return null;
        }

        if (startPtY >= (yDim - 1)) {
            return null;
        }


        x = (int) (startPtX + 0.5);
        y = (int) (startPtY + 0.5);
        
        float level = imageBufferActive[ y * xDim + x ];

        index = (y * xDim) + x;

        levelSetStack.reset();
        levelSetStack.addPoint(x, y);
        map.set((y * xDim) + x);

        int dir = -1;
        float diff = 100000;

        do {
            index = (y * xDim) + x;

            if ((x >= 2) && (x < (xDim - 2)) && (y >= 2) && (y < (yDim - 2))) {

                if ((avgPix(index - xDim) >= level) &&
                        ((avgPix(index - xDim + 1) < level) || (avgPix(index) < level) ||
                                (avgPix(index - xDim - 1) < level) || (avgPix(index - (2 * xDim)) < level)) &&
                                (map.get(index - xDim) == false)) {
                    dir = 1;
                    diff = Math.abs(avgPix(index - xDim) - avgPix(index));
                }

                if ((avgPix(index - xDim + 1) >= level) &&
                        ((avgPix(index - xDim + 2) < level) || (avgPix(index + 1) < level) ||
                                (avgPix(index - xDim) < level) || (avgPix(index - (2 * xDim) + 1) < level)) &&
                                (map.get(index - xDim + 1) == false)) {

                    if (Math.abs(avgPix(index - xDim + 1) - avgPix(index)) < diff) {
                        dir = 2;
                        diff = Math.abs(avgPix(index - xDim + 1) - avgPix(index));
                    }
                }

                if ((avgPix(index + 1) >= level) &&
                        ((avgPix(index + 2) < level) || (avgPix(index + xDim + 1) < level) || (avgPix(index) < level) ||
                                (avgPix(index - xDim + 1) < level)) && (map.get(index + 1) == false)) {

                    if (Math.abs(avgPix(index + 1) - avgPix(index)) < diff) {
                        dir = 3;
                        diff = Math.abs(avgPix(index + 1) - avgPix(index));
                    }
                }

                if ((avgPix(index + xDim + 1) >= level) &&
                        ((avgPix(index + xDim + 2) < level) || (avgPix(index + (2 * xDim) + 1) < level) ||
                                (avgPix(index + 1) < level) || (avgPix(index + xDim) < level)) &&
                                (map.get(index + xDim + 1) == false)) {

                    if (Math.abs(avgPix(index + xDim + 1) - avgPix(index)) < diff) {
                        dir = 4;
                        diff = Math.abs(avgPix(index + xDim + 1) - avgPix(index));
                    }
                }

                if ((avgPix(index + xDim) >= level) &&
                        ((avgPix(index + xDim + 1) < level) || (avgPix(index + (2 * xDim)) < level) ||
                                (avgPix(index + xDim - 1) < level) || (avgPix(index) < level)) &&
                                (map.get(index + xDim) == false)) {

                    if (Math.abs(avgPix(index + xDim) - avgPix(index)) < diff) {
                        dir = 5;
                        diff = Math.abs(avgPix(index + xDim) - avgPix(index));
                    }
                }

                if ((avgPix(index + xDim - 1) >= level) &&
                        ((avgPix(index + xDim) < level) || (avgPix(index + (2 * xDim) - 1) < level) ||
                                (avgPix(index + xDim - 2) < level) || (avgPix(index - 1) < level)) &&
                                (map.get(index + xDim - 1) == false)) {

                    if (Math.abs(avgPix(index + xDim - 1) - avgPix(index)) < diff) {
                        dir = 6;
                        diff = Math.abs(avgPix(index + xDim - 1) - avgPix(index));
                    }
                }

                if ((avgPix(index - 1) >= level) &&
                        ((avgPix(index) < level) || (avgPix(index + xDim - 1) < level) || (avgPix(index - 2) < level) ||
                                (avgPix(index - xDim - 1) < level)) && (map.get(index - 1) == false)) {

                    if (Math.abs(avgPix(index - 1) - avgPix(index)) < diff) {
                        dir = 7;
                        diff = Math.abs(avgPix(index - 1) - avgPix(index));
                    }
                }

                if ((avgPix(index - xDim - 1) >= level) &&
                        ((avgPix(index - xDim) < level) || (avgPix(index - 1) < level) ||
                                (avgPix(index - xDim - 2) < level) || (avgPix(index - (2 * xDim) - 1) < level)) &&
                                (map.get(index - xDim - 1) == false)) {

                    if (Math.abs(avgPix(index - xDim - 1) - avgPix(index)) < diff) {
                        dir = 0;
                        // diff = Math.abs(imageBufferActive[index-xDim-1] - imageBufferActive[index]);
                    }
                }

                diff = 1000000;

                if (dir == 1) {
                    // x = x;
                    y = y - 1;
                    map.set(index - xDim);
                    paths(index, 1, level);
                } else if (dir == 2) {
                    x = x + 1;
                    y = y - 1;
                    map.set(index - xDim + 1);
                    paths(index, 2, level);
                } else if (dir == 3) {
                    x = x + 1;
                    // y = y;
                    map.set(index + 1);
                    paths(index, 3, level);
                } else if (dir == 4) {
                    x = x + 1;
                    y = y + 1;
                    map.set(index + xDim + 1);
                    paths(index, 4, level);
                } else if (dir == 5) {
                    // x = x;
                    y = y + 1;
                    map.set(index + xDim);
                    paths(index, 5, level);
                } else if (dir == 6) {
                    x = x - 1;
                    y = y + 1;
                    map.set(index + xDim - 1);
                    paths(index, 6, level);
                } else if (dir == 7) {
                    x = x - 1;
                    // y = y;
                    map.set(index - 1);
                    paths(index, 7, level);
                } else if (dir == 0) {
                    x = x - 1;
                    y = y - 1;
                    map.set(index - xDim - 1);
                    paths(index, 0, level);
                } else {

                    if (!stack.empty()) {
                        int ptr = (stack.pop())[0];
                        x = levelSetStack.getPointX(ptr);
                        y = levelSetStack.getPointY(ptr);
                        levelSetStack.setIndex(ptr);
                    } else {
                        x = y = -1;
                    }
                }

                dir = -1;
            } else { // near edge of image
                levelSetStack.reset();

                break;
            }

            if ((x == -1) || (y == -1)) {
                levelSetStack.reset();

                break;
            }

            levelSetStack.addPoint(x, y);

            distance = ((x - startPtX) * (x - startPtX)) + ((y - startPtY) * (y - startPtY));

            if ((distance < 2.1) && (levelSetStack.size() < 10)) {
                distance = 10;
            }
        } while (distance > 2.1);



        if (levelSetStack.size() != 0) {
            new Vector3f();
            Vector3f kScreenPt = new Vector3f();

            Vector<Vector3f> kPositions = new Vector<Vector3f>();
            for ( int i = 0; i < levelSetStack.size(); i++ )
            {
                kPatientPt.Set( levelSetStack.getPointX(i), levelSetStack.getPointY(i), m_iSlice );
                kScreenPt = m_kDrawingContext.patientToScreen( kPatientPt );
                kScreenPt.Y = kScreenPt.Y;
                kPositions.add( kScreenPt );
            }
            kPatientPt.Set( levelSetStack.getPointX(0), levelSetStack.getPointY(0), m_iSlice );   
            kScreenPt = m_kDrawingContext.patientToScreen( kPatientPt );
            kScreenPt.Y = kScreenPt.Y;
            kPositions.add( kScreenPt );
            return createVOI( m_iDrawType, true, true, kPositions );
        } 
        return null;
    }
    
    
    
    
    
}