package gov.nih.mipav.view.renderer.WildMagic.VOI;

import gov.nih.mipav.MipavCoordinateSystems;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.PointStack;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.RubberbandLivewire;
import gov.nih.mipav.view.dialogs.JDialogAnnotation;
import gov.nih.mipav.view.dialogs.JDialogVOISplitter;
import gov.nih.mipav.view.renderer.WildMagic.Render.LocalVolumeVOI;
import gov.nih.mipav.view.renderer.WildMagic.Render.VOIContour3D;
import gov.nih.mipav.view.renderer.WildMagic.Render.VOILine3D;
import gov.nih.mipav.view.renderer.WildMagic.Render.VOIPoint3D;
import gov.nih.mipav.view.renderer.WildMagic.Render.VOIProtractor3D;
import gov.nih.mipav.view.renderer.WildMagic.Render.VOIText3D;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.io.IOException;
import java.util.BitSet;
import java.util.Stack;
import java.util.Vector;

import javax.swing.KeyStroke;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

public class VOIManager implements KeyListener, MouseListener, MouseMotionListener
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

    private static final int TEXT = 0;
    private static final int POINT = 1;
    public static final int POLYPOINT = 2;
    private static final int LINE = 3;
    private static final int PROTRACTOR = 4;
    private static final int RECTANGLE = 5;
    private static final int OVAL = 6;
    public static final int POLYLINE = 7;
    public static final int LEVELSET = 8;
    public static final int LIVEWIRE = 9;
    private static final int RECTANGLE3D = 10;
    public static final int SPLITLINE = 11;
    private int m_iDrawType;

    private LocalVolumeVOI m_kCurrentVOI = null;
    private LocalVolumeVOI m_kCopyVOI = null;
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

    private byte[] m_aucBufferA;
    private byte[] m_aucBufferB;
    private byte[] m_aucBufferActive;

    private int[] m_aiLocalImageExtents;

    private VOIManagerListener m_kParent;

    private Vector2f[][] m_akSteps = new Vector2f[7][7];

    private int[][] m_aiIndexValues = new int[7][7];   

    private float[][] m_afAverages = new float[7][7];


    private int m_iMM = 1;

    private int m_iM = 2;

    private int m_i_ = 3;


    private int m_iP = 4;
    private int m_iPP = 5;
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
    
    public VOIManager (VOIManagerListener kParent )
    {
        m_kParent = kParent;
    }

    public void anchor(int iX, int iY, boolean bSeed) {
        Vector3f kNewPoint = new Vector3f( iX, iY, m_iSlice ) ;
        if ( m_kCurrentVOI == null )
        {
            Vector<Vector3f> kPositions = new Vector<Vector3f>();
            kPositions.add( kNewPoint );
            m_kCurrentVOI = new VOIContour3D( this, m_kDrawingContext, m_iPlaneOrientation, m_iDrawType, m_iDrawType, kPositions, false);
            m_kParent.addVOI( m_kCurrentVOI, true );
        }
        else
        {
            if ( m_kCurrentVOI.add( kNewPoint, false ) )
            {
                m_kParent.updateDisplay();
            }
        }
        m_kCurrentVOI.setAnchor();
        if ( bSeed )
        {
            initLiveWire( m_iSlice, true );
            seed(iX, iY);
        }
    }

    public void anchorPolyline(int iX, int iY, boolean bFinished) {
        Vector3f kNewPoint = new Vector3f( iX, iY, m_iSlice ) ;
        if ( m_kCurrentVOI == null )
        {
            Vector<Vector3f> kPositions = new Vector<Vector3f>();
            kPositions.add( kNewPoint );
            m_kCurrentVOI = new VOIContour3D( this, m_kDrawingContext, m_iPlaneOrientation, m_iDrawType, m_iDrawType, kPositions, false);
            m_kParent.addVOI( m_kCurrentVOI, true );
        }
        else
        {
            if ( m_kCurrentVOI.add( kNewPoint, false ) )
            {
                m_kParent.updateDisplay();
            }
        }
        m_kCurrentVOI.setAnchor();
        if ( !bFinished )
        {
            m_kCurrentVOI.setActive(true);
        }
    }

    public int deleteVOIActivePt( LocalVolumeVOI kVOI )
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
        m_aucBufferA = null;
        m_aucBufferB = null;
        m_aucBufferActive = null;

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

        m_akSteps = null;
        m_aiIndexValues = null;
        m_afAverages = null;

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


        if (kCommand.equals("Pointer") ) {
            m_bPointer = true;
        }
        else if ( kCommand.equals("NewVOI") )
        {
            m_kCurrentVOI = null;
        }
        else if ( isDrawCommand )
        {
            m_bDrawVOI = true;
            m_bSelected = false;
            if ( m_kCurrentVOI != null )
            {
                m_kCurrentVOI.setActive(false);
                m_kCurrentVOI = null;
            }

            if ( kCommand.equals("protractor") )
            {
                m_iDrawType = PROTRACTOR;
            }
            else if ( kCommand.equals("LiveWireVOI") )
            {
                m_iDrawType = LIVEWIRE;
            }
            else if ( kCommand.equals("SplitVOI") )
            {
                m_iDrawType = SPLITLINE;
            }
            else if ( kCommand.equals("Line") )
            {
                m_iDrawType = LINE;
            }
            else if ( kCommand.equals("Polyslice") )
            {
                m_iDrawType = POLYPOINT;
            }
            else if ( kCommand.equals("Point") ) {
                m_iDrawType = POINT;
            }
            else if ( kCommand.equals("TextVOI") ) {
                m_iDrawType = TEXT;
            }
            else if (kCommand.equals("Rect3DVOI") ) {
                m_iDrawType = RECTANGLE3D;
            } 
            else if (kCommand.equals("RectVOI") ) {
                m_iDrawType = RECTANGLE;
            } 
            else if (kCommand.equals("EllipseVOI") ) {
                m_iDrawType = OVAL;
            } 
            else if (kCommand.equals("Polyline") ) {
                m_iDrawType = POLYLINE;
            } 
            else if (kCommand.equals("LevelSetVOI") ) {
                m_iDrawType = LEVELSET;
            } 
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
        if ( m_kCurrentVOI.add( kNewPoint, false ) )
        {
            m_kParent.updateDisplay( );
        }

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
            if ( m_kCurrentVOI.add( iAnchor, kVolumePt, true ) )
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
        if ( m_kCurrentVOI.add( iAnchor, kVolumePt, true ) )
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
        while ( m_kCurrentVOI.getAnchor() < (m_kCurrentVOI.size()-1) )
        {
            m_kCurrentVOI.delete(m_kCurrentVOI.getAnchor()+1);
        }

        float fY = iY;
        Vector3f kNewPoint = new Vector3f( iX, fY, m_iSlice ) ;
        if ( m_kCurrentVOI.add( kNewPoint, false ) )
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

    public void fillVolume( Vector<Vector3f> kPositions, ModelImage kVolume, BitSet kMask, boolean bIntersection, int iValue )
    {
        int iNumPoints = kPositions.size();
        if ( iNumPoints == 0 )
        {
            return;
        }
        Vector3f[] kVolumePts = new Vector3f[iNumPoints + 1];
        int iXMin = Integer.MAX_VALUE;
        int iYMin = Integer.MAX_VALUE;
        int iXMax = Integer.MIN_VALUE;
        int iYMax = Integer.MIN_VALUE;

        for ( int i = 0; i < iNumPoints; i++ )
        {
            Vector3f kVolumePt = kPositions.get(i);
            Vector3f kPt = new Vector3f();
            MipavCoordinateSystems.fileToPatient( kVolumePt, kPt, m_kImageActive, m_iPlaneOrientation );

            //kPt.Z = iSlice;/
            kVolumePts[i] = kPt;
            iXMin = (int)Math.min( iXMin, kVolumePts[i].X );
            iYMin = (int)Math.min( iYMin, kVolumePts[i].Y );
            iXMax = (int)Math.max( iXMax, kVolumePts[i].X );
            iYMax = (int)Math.max( iYMax, kVolumePts[i].Y );
        }
        Vector3f kVolumePt = kPositions.get(0);
        Vector3f kPt = new Vector3f();
        MipavCoordinateSystems.fileToPatient( kVolumePt, kPt, m_kImageActive, m_iPlaneOrientation );

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

        outlineRegion(aaiCrossingPoints, aiNumCrossings, iXMin, iYMin, iXMax, iYMax, kVolumePts, kVolume);
        fill(aaiCrossingPoints, aiNumCrossings, iXMin, iYMin, iXMax, iYMax, (int)kVolumePts[0].Z, kVolume, kMask, bIntersection, iValue);

    }

    public ModelImage[] getActiveImage()
    {
        return m_akImages;
    }

    public LocalVolumeVOI getCurrentVOI()
    {
        return m_kCurrentVOI;
    }


    public int getOrientation()
    {
        return m_iPlaneOrientation;
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
    }


    public boolean isActive()
    {
        return (m_bDrawVOI || m_bPointer);
    }

    public void keyPressed(KeyEvent e) {
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
        final int keyCode = e.getKeyCode();
        switch (keyCode) {
        case KeyEvent.VK_DELETE:
            if (e.isShiftDown()) 
            {
                m_kParent.doVOI("deleteVOIActivePt");
            } 
            else 
            {
                m_kParent.doVOI("deleteVOI");
            }
            return;
        }
    }

    public void keyTyped(KeyEvent e) {}




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
                        kGroup.getCurves()[0].remove(m_kCurrentVOI);
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
            m_bDrawVOI = false;
            m_iNearStatus = NearNone;
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
            m_kParent.setActive(this);
        }        
    }

    public void mouseExited(MouseEvent arg0) {}

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
            m_kParent.doVOI("PropVOIAll");
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
            return;
        }
        else if ( m_bDrawVOI && (m_iDrawType == LIVEWIRE) )
        {
            anchor( kEvent.getX(), kEvent.getY(), true );
            return;
        }
        else if ( m_bDrawVOI && (m_iDrawType == POLYLINE) )
        {
            anchorPolyline( kEvent.getX(), kEvent.getY(), false );
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

    public void pasteAllVOI( LocalVolumeVOI kVOI )
    {
        m_kCopyVOI = kVOI;
        for ( int i = 0; i < m_aiLocalImageExtents[2]; i++ )
        {
            pasteVOI(i);
        }
        m_kCurrentVOI = null;
    }


    public void pasteVOI( LocalVolumeVOI kVOI )
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
        if ( m_kImageActive == m_akImages[0] )
        {
            m_aucBufferActive = m_aucBufferA;
        }
        else
        {
            m_aucBufferActive = m_aucBufferB;
        }
    }

    public void setCanvas (Component kComponent)
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

    public void setCurrentVOI( LocalVolumeVOI kCurrentVOI )
    {
        m_kCurrentVOI = kCurrentVOI;
    }
    public void setDataBuffers(byte[] bufferA, byte[] bufferB)
    {
        m_aucBufferA = bufferA;
        m_aucBufferB = bufferB;
        if ( m_kImageActive == m_akImages[0] )
        {
            m_aucBufferActive = m_aucBufferA;
        }
        else
        {
            m_aucBufferActive = m_aucBufferB;
        }
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

    /**
     * Sets the local slice value.
     * @param fSlice
     */
    public void setSlice(float fSlice) {
        int iSlice = (int)fSlice;

        /* Check bounds: */
        if (iSlice > (m_aiLocalImageExtents[2] - 1)) {
            iSlice = m_aiLocalImageExtents[2] - 1;
        }

        if (iSlice < 0) {
            iSlice = 0;
        }

        if (iSlice != m_iSlice) {
            m_iSlice = iSlice;
        }
    }

    /**
     * fill: fill the sculpt outline drawn by the user. Pixels are determined to be inside or outside the sculpt region
     * based on the parameters, aaiCrossingPoints and aiNumCrossings, using a scan-conversion algorithm that traverses
     * each row and column of the bounding box of the sculpt region coloring inside points as it goes.
     *
     * @param  aaiCrossingPoints  DOCUMENT ME!
     * @param  aiNumCrossings     DOCUMENT ME!
     */
    protected void fill(int[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iXMin, int iYMin, int iXMax, int iYMax, int iZ,
            ModelImage kVolume, BitSet kMask, boolean bIntersection, int iValue)
    {
        Vector3f kLocalPt = new Vector3f();
        Vector3f kVolumePt = new Vector3f();
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
                    kLocalPt.Set(iX, iY, iZ);
                    //kVolumePt = VOIToFileCoordinates(kLocalPt, false);
                    MipavCoordinateSystems.patientToFile( kLocalPt, kVolumePt, m_kImageActive, m_iPlaneOrientation );

                    if ( bIntersection )
                    {
                        int iTemp = kVolume.getInt( (int)kVolumePt.X, (int)kVolumePt.Y, (int)kVolumePt.Z );

                        if ( kMask != null )
                        {
                            int iIndex = (int)kVolumePt.Z * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                            iIndex += kVolumePt.Y * kVolume.getExtents()[0];
                            iIndex += kVolumePt.X;
                            if ( iValue == 0 )
                            {
                                kMask.set( iIndex );
                            }
                            else if ( kMask.get( iIndex ) )
                            {
                                kMask.set( iIndex );
                            }
                        }
                        else
                        {
                            if ( iValue == 0 )
                            {
                                kVolume.set( (int)kVolumePt.X, (int)kVolumePt.Y, (int)kVolumePt.Z, 85 );
                            }
                            else if ( iTemp != 0 )
                            {
                                kVolume.set( (int)kVolumePt.X, (int)kVolumePt.Y, (int)kVolumePt.Z, 255 );
                            }
                        }
                    }
                    else
                    {
                        if ( kMask != null )
                        {
                            int iIndex = (int)kVolumePt.Z * kVolume.getExtents()[0] * kVolume.getExtents()[1];
                            iIndex += kVolumePt.Y * kVolume.getExtents()[0];
                            iIndex += kVolumePt.X;
                            kMask.set( iIndex );
                        }
                        else
                        {
                            kVolume.set( (int)kVolumePt.X, (int)kVolumePt.Y, (int)kVolumePt.Z, 255 );
                        }
                    }
                }
            }

            iColumn++;
        }
    }

    /**
     * This function computes the set of spans indicated by column crossings for the sculpt outline drawn by the user,
     * by doing a polygon scan conversion in gridded space. The outline must be closed with last point = first point.
     *
     * @param  aaiCrossingPoints  DOCUMENT ME!
     * @param  aiNumCrossings     DOCUMENT ME!
     */
    protected void outlineRegion(int[][] aaiCrossingPoints, int[] aiNumCrossings,
            int iXMin, int iYMin, int iXMax, int iYMax,
            Vector3f[] kVolumePts, ModelImage kVolume)
    {
        int iNumPts = kVolumePts.length;

        /*
         * nudge the vertices off of the exact integer coords by a factor of 0.1 to avoid vertices on pixel centers,
         * which would create spans of zero length
         */
        double dNudge = 0.1;       
        double[][][] aaadEdgeList = new double[iNumPts][2][2];

        for (int iPoint = 0; iPoint < (iNumPts - 1); iPoint++) {
            aaadEdgeList[iPoint][0][0] = kVolumePts[iPoint].X - dNudge;
            aaadEdgeList[iPoint][0][1] = kVolumePts[iPoint].Y - dNudge;
            aaadEdgeList[iPoint][1][0] = kVolumePts[iPoint + 1].X - dNudge;
            aaadEdgeList[iPoint][1][1] = kVolumePts[iPoint + 1].Y - dNudge;
        }

        /*
         * Compute the crossing points for this column and produce spans.
         */
        for (int iColumn = iXMin; iColumn <= iXMax; iColumn++) {
            int iIndex = iColumn - iXMin;

            /* for each edge, figure out if it crosses this column and add its
             * crossing point to the list if so. */
            aiNumCrossings[iIndex] = 0;

            for (int iPoint = 0; iPoint < (iNumPts - 1); iPoint++) {
                double dX0 = aaadEdgeList[iPoint][0][0];
                double dX1 = aaadEdgeList[iPoint][1][0];
                double dY0 = aaadEdgeList[iPoint][0][1];
                double dY1 = aaadEdgeList[iPoint][1][1];
                double dMinX = (dX0 <= dX1) ? dX0 : dX1;
                double dMaxX = (dX0 > dX1) ? dX0 : dX1;

                if ((dMinX < iColumn) && (dMaxX > iColumn)) {

                    /* The edge crosses this column, so compute the
                     * intersection.
                     */
                    double dDX = dX1 - dX0;
                    double dDY = dY1 - dY0;
                    double dM = (dDX == 0) ? 0 : (dDY / dDX);
                    double dB = (dDX == 0) ? 0 : (((dX1 * dY0) - (dY1 * dX0)) / dDX);

                    double dYCross = (dM * iColumn) + dB;
                    double dRound = 0.5;
                    aaiCrossingPoints[iIndex][aiNumCrossings[iIndex]] = (dYCross < 0) ? (int) (dYCross - dRound)
                            : (int) (dYCross + dRound);
                    aiNumCrossings[iIndex]++;
                }
            }

            /* sort the set of crossings for this column: */
            sortCrossingPoints(aaiCrossingPoints[iIndex], aiNumCrossings[iIndex]);
        }

        aaadEdgeList = null;
    }

    /**
     * Sorts the edge crossing points in place.
     *
     * @param  aiList        list of positions
     * @param  iNumElements  number of positions.
     */
    protected void sortCrossingPoints(int[] aiList, int iNumElements) {
        boolean bDidSwap = true;

        while (bDidSwap) {
            bDidSwap = false;

            for (int iPoint = 0; iPoint < (iNumElements - 1); iPoint++) {

                if (aiList[iPoint] > aiList[iPoint + 1]) {
                    int iTmp = aiList[iPoint];
                    aiList[iPoint] = aiList[iPoint + 1];
                    aiList[iPoint + 1] = iTmp;
                    bDidSwap = true;
                }
            }
        }
    }



    private void addVOIPoint( int iX, int iY )
    {
        if ( m_kCurrentVOI == null )
        {            
            return;
        }     
        int iPos = m_kCurrentVOI.getNearPoint();
        if ( m_kCurrentVOI.add( iPos, new Vector3f(iX, iY, m_iSlice), false ) )
        {
            m_kParent.setCursor(MipavUtil.crosshairCursor);        
            m_iNearStatus = NearPoint;
            m_kParent.updateDisplay();
        }
    }
    /**
     * This method calculates the average pixel value based on the four neighbors (N, S, E, W).
     *
     * @param   index  the center pixel where the average pixel value is to be calculated.
     *
     * @return  the average pixel value as a float.
     */
    private float avgPix( int iX, int iY )
    {
        int index = m_aiIndexValues[iY][iX];
        int[] extents = m_kImageActive.getExtents();
        if ((index > extents[0]) && (index < (m_kImageActive.getSize() - extents[0]))) {

            int sum = (m_aucBufferActive[index] & 0x00ff);

            index = m_aiIndexValues[iY-1][iX];
            sum += (m_aucBufferActive[index] & 0x00ff);

            index = m_aiIndexValues[iY][iX-1];
            sum += (m_aucBufferActive[index] & 0x00ff);

            index = m_aiIndexValues[iY][iX+1];
            sum += (m_aucBufferActive[index] & 0x00ff);

            index = m_aiIndexValues[iY+1][iX];
            sum += (m_aucBufferActive[index] & 0x00ff);
            return sum / 5.0f;
        } 
        return (m_aucBufferActive[index] & 0x00ff);
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
        VOI newTextVOI = new VOI((short) colorID, "annotation3d.voi",
                m_kImageActive.getExtents()[2], VOI.ANNOTATION, -1.0f);


        Vector<Vector3f> kPositions = new Vector<Vector3f>();
        kPositions.add( new Vector3f (iX, iY, m_iSlice));


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

        kPositions.add( new Vector3f (iX2, iY2, m_iSlice));
        m_kCurrentVOI = new VOIText3D( this, m_kDrawingContext, m_iPlaneOrientation, m_iDrawType, kPositions, false);


        float[] x = new float[2];
        float[] y = new float[2];
        float[] z = new float[2];
        for ( int i = 0; i < 2; i++ )
        {
            x[i] = m_kCurrentVOI.get(i).X;
            y[i] = m_kCurrentVOI.get(i).Y;
            z[i] = m_kCurrentVOI.get(i).Z;
        }

        int iFileSlice = (int)z[0];
        newTextVOI.importCurve(x, y, z, iFileSlice);
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
        new JDialogAnnotation(m_kImageActive, newTextVOI, iFileSlice, false, true);
        if ( newTextVOI.isActive() ) {
            VOIText vt = (VOIText) newTextVOI.getCurves()[iFileSlice].elementAt(0);
            ((VOIText3D)m_kCurrentVOI).copyInfo(vt);
            m_kCurrentVOI.setLabel(vt.getName());
            m_kCurrentVOI.setActive(true);
            m_kParent.addVOI( m_kCurrentVOI, true );
        }
        else
        {
            m_kCurrentVOI = null;
        }

    }

    private void createVOI( int iX, int iY )
    {
        float fYStart = m_fMouseY;
        float fY = iY;

        LocalVolumeVOI kOld = m_kCurrentVOI;
        if ( (m_iDrawType == POINT) || (m_iDrawType == POLYPOINT) )
        { 
            Vector<Vector3f> kPositions = new Vector<Vector3f>();
            kPositions.add( new Vector3f (iX, fY, m_iSlice ) );

            m_kCurrentVOI = new VOIPoint3D( this, m_kDrawingContext, m_iPlaneOrientation, m_iDrawType, kPositions, false);

        }
        else if ( m_iDrawType == RECTANGLE || m_iDrawType == RECTANGLE3D )
        {
            if ( m_kCurrentVOI == null )
            {            
                Vector<Vector3f> kPositions = new Vector<Vector3f>();
                kPositions.add( new Vector3f (m_fMouseX, fYStart, m_iSlice));
                kPositions.add( new Vector3f (iX, fYStart, m_iSlice));
                kPositions.add( new Vector3f (iX, fY, m_iSlice));
                kPositions.add( new Vector3f (m_fMouseX, fY, m_iSlice));
                m_kCurrentVOI = new VOIContour3D( this, m_kDrawingContext, m_iPlaneOrientation, m_iDrawType, m_iDrawType, kPositions, false);
            }
            else
            {
                m_kCurrentVOI.setPosition( this, 1, iX, fYStart, m_iSlice);
                m_kCurrentVOI.setPosition( this, 2, iX, fY, m_iSlice);
                m_kCurrentVOI.setPosition( this, 3, m_fMouseX, fY, m_iSlice);       
            }
        }
        else if ( m_iDrawType == OVAL )
        {
            float fRadiusX = Math.abs(m_fMouseX - iX);
            float fRadiusY = Math.abs(fYStart - fY);
            if ( m_kCurrentVOI == null )
            {            
                Vector<Vector3f> kPositions = new Vector<Vector3f>();
                for ( int i = 0; i < m_iCirclePts; i++ )
                {
                    kPositions.add( new Vector3f ((float)(m_fMouseX + fRadiusX * m_adCos[i]),
                            (float)(fYStart + fRadiusY * m_adSin[i]), m_iSlice));
                }
                m_kCurrentVOI = new VOIContour3D( this, m_kDrawingContext, m_iPlaneOrientation, m_iDrawType, m_iDrawType, kPositions, false );
            }
            else
            {
                for ( int i = 0; i < m_iCirclePts; i++ )
                {
                    m_kCurrentVOI.setPosition( this, i, (float)(m_fMouseX + fRadiusX * m_adCos[i]),
                            (float)(fYStart + fRadiusY * m_adSin[i]), m_iSlice);
                }
            }
        }
        else if ( m_iDrawType == LEVELSET )
        {
            //initLiveWire( m_iSlice, false );
            LocalVolumeVOI kTemp = singleLevelSet(iX, fY);
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

                m_kCurrentVOI = new VOIContour3D( this, m_kDrawingContext, m_iPlaneOrientation, m_iDrawType, m_iDrawType, kPositions, false);
            }
            else
            {
                Vector3f kNewPoint = new Vector3f( iX, fY, m_iSlice ) ;
                if ( m_kCurrentVOI.add( kNewPoint, false ) )
                {
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

                m_kCurrentVOI = new VOILine3D( this, m_kDrawingContext, m_iPlaneOrientation, m_iDrawType, kPositions, false);
                if ( m_iDrawType == SPLITLINE )
                {
                    m_kCurrentVOI.setSType( SPLITLINE );
                }
            }
            else
            {
                Vector3f kNewPoint = new Vector3f( iX, fY, m_iSlice ) ;
                m_kCurrentVOI.setPosition( this, 1, kNewPoint );
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

                m_kCurrentVOI = new VOIProtractor3D( this, m_kDrawingContext, m_iPlaneOrientation, m_iDrawType, kPositions, false);
            }
            else
            {
                Vector3f kStart = m_kDrawingContext.fileToScreen(m_kCurrentVOI.get(1));
                Vector3f kEnd = new Vector3f( iX, fY, m_iSlice ) ;
                Vector3f kMiddle = new Vector3f();
                kMiddle.Sub( kEnd, kStart );
                kMiddle.Scale( .4f );
                kMiddle.Add(kStart);

                m_kCurrentVOI.setPosition( this, 0, kMiddle );
                m_kCurrentVOI.setPosition( this, 2, kEnd );
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
    }

    private void initLiveWire( int iSlice, boolean bLiveWire )
    {
        m_kParent.setCursor( MipavUtil.waitCursor );
        if ( !m_bLiveWireInit )
        {
            int[] aiAxisOrder = MipavCoordinateSystems.getAxisOrder(m_kImageActive, m_iPlaneOrientation);
            boolean[] abAxisFlip = MipavCoordinateSystems.getAxisFlip(m_kImageActive, m_iPlaneOrientation);
            m_kLocalImage = m_kImageActive.export( aiAxisOrder, abAxisFlip );

            m_bLiveWireInit = true;
            m_abInitLiveWire = new boolean[m_aiLocalImageExtents[2] ];
            for ( int i = 0; i < m_abInitLiveWire.length; i++ )
            {
                m_abInitLiveWire[i] = false;
            }
        }
        if ( m_abInitLiveWire[iSlice] )
        {
            m_kParent.setDefaultCursor();
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
            localCosts = RubberbandLivewire.getLocalCosts( m_kLocalImage, m_iLiveWireSelection, 
                    imageBufferActive,
                    xDirections, yDirections, null );

            costGraph = new byte[localCosts.length]; // Graph with arrows from each node to next one

            // A node is a location i in the array; where it
            // points to is the value costGraph[i].
            processedIndicies = new BitSet(localCosts.length); // Boolean indicating if pixel at location
            // has been processed.
            seededCosts = new float[localCosts.length]; // Seeded costs, so looking up a cost that has been
            // set is easy
            activeTree = new ActiveTree(); // List of active nodes to expand; reset on seed(pt) call


            m_abInitLiveWire[iSlice] = true;
        }
        m_kParent.setDefaultCursor();
    }

    private void moveVOIPoint( int iX, int iY )
    {
        if ( m_kCurrentVOI == null )
        {            
            return;
        }
        m_kCurrentVOI.setPosition( this, m_kCurrentVOI.getNearPoint(), iX, iY, m_iSlice );  

        m_kParent.setCursor(MipavUtil.crosshairCursor);
    }

    private void pasteVOI( int iSlice )
    {
        if ( m_kCopyVOI == null )
        {
            return;
        }
        m_kCurrentVOI = m_kCopyVOI.clone(iSlice);
        m_kParent.pasteVOI(m_kCurrentVOI);
        m_kCurrentVOI.setActive(false);
    }

    /**
     * Generates the possible paths of the level set and pushes them onto a stack. Looks in the 8 neighborhood
     * directions for the possible paths.
     *
     */
    private void paths(int iX, int iY, int iZ, int i, float level) {

        int[] intPtr = null;

        try {
            intPtr = new int[1];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.mouseDragged");

            return;
        }

        intPtr[0] = levelSetStack.size() - 1;

        //indexMM
        if ((i != 0) && (m_afAverages[m_iM][m_iM] <= level) && (map.get(m_aiIndexValues[m_iM][m_iM]) == false)) {
            stack.push(intPtr);
        } 
        //indexM_
        else if ((i != 1) && (m_afAverages[m_iM][m_i_] <= level) && (map.get(m_aiIndexValues[m_iM][m_i_]) == false)) {
            stack.push(intPtr);
        }
        //indexMP
        else if ((i != 2) && (m_afAverages[m_iM][m_iP] <= level) && (map.get(m_aiIndexValues[m_iM][m_iP]) == false)) {
            stack.push(intPtr);
        } 
        //index_P
        else if ((i != 3) && (m_afAverages[m_i_][m_iP] <= level) && (map.get(m_aiIndexValues[m_i_][m_iP]) == false)) {
            stack.push(intPtr);
        }
        //indexPP
        else if ((i != 4) && (m_afAverages[m_iP][m_iP] <= level) && (map.get(m_aiIndexValues[m_iP][m_iP]) == false)) {
            stack.push(intPtr);
        }
        //indexP_
        else if ((i != 5) && (m_afAverages[m_iP][m_i_] <= level) && (map.get(m_aiIndexValues[m_iP][m_i_]) == false)) {
            stack.push(intPtr);
        }
        //indexPM
        else if ((i != 6) && (m_afAverages[m_iP][m_iM] <= level) && (map.get(m_aiIndexValues[m_iP][m_iM]) == false)) {
            stack.push(intPtr);
        }
        // index_M
        else if ((i != 7) && (m_afAverages[m_i_][m_iM] <= level) && (map.get(m_aiIndexValues[m_i_][m_iM]) == false)) {
            stack.push(intPtr);
        }
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
                m_kParent.moveVOI( new Vector3f( kEvent.getX() - m_fMouseX, kEvent.getY() - m_fMouseY, 0 ) );
                m_fMouseX = kEvent.getX();
                m_fMouseY = kEvent.getY();
            }
        }
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

    private void selectVOI( int iX, int iY, boolean bShiftDown )
    {
        if ( m_kCurrentVOI != null )
        {
            m_kCurrentVOI.setActive(false);
        }
        m_bSelected = false;
        m_kCurrentVOI = null;
        VOIVector kVOIs = m_kImageActive.get3DVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kVOI = kVOIs.get(i);
            for ( int j = 0; j < kVOI.getCurves()[0].size(); j++ )
            {
                LocalVolumeVOI kVOI3D = ((LocalVolumeVOI)kVOI.getCurves()[0].get(j));
                if ( kVOI3D.contains( m_iPlaneOrientation, iX, iY, m_iSlice ) )
                {
                    m_kCurrentVOI = kVOI3D;
                    m_kParent.setSelectedVOI( m_kCurrentVOI.getGroup(), bShiftDown );
                    m_kCurrentVOI.setActive(true);
                    m_bSelected = true;
                    return;
                }
            }
        }
    }




    private void setIndices( int iX, int iY, int iZ )
    {
        for ( int i = 0; i < 7; i++ )
        {
            for ( int j = 0; j < 7; j++ )
            {
                if ( m_akSteps[i][j] == null )
                {
                    m_akSteps[i][j] = new Vector2f( iX + j-3, iY + i - 3 );
                }
                else
                {
                    m_akSteps[i][j].Set( iX + j-3, iY + i - 3 );
                }
            }
        }

        int[] extents = m_kImageActive.getExtents();  
        Vector3f kVolumePt = new Vector3f();
        Vector3f kPatientPt = new Vector3f();    

        for ( int i = 0; i < 7; i++ )
        {
            for ( int j = 0; j < 7; j++ )
            {
                kPatientPt.Set( Math.min( m_aiLocalImageExtents[0]-1, Math.max( 0, m_akSteps[i][j].X ) ),
                        Math.min( m_aiLocalImageExtents[1]-1, Math.max( 0, m_akSteps[i][j].Y ) ), iZ );
                MipavCoordinateSystems.patientToFile( kPatientPt, kVolumePt, m_kImageActive, m_iPlaneOrientation );     
                m_aiIndexValues[i][j] = (int)(kVolumePt.Z * extents[0] * extents[1] + kVolumePt.Y * extents[0] + kVolumePt.X);
            }
        }

        for ( int i = 1; i < 6; i++ )
        { 
            for ( int j = 1; j < 6; j++ )
            {
                m_afAverages[i][j] = avgPix( j, i );
            }
        }
    }


    private void showSelectedVOI( int iX, int iY )
    {
        if ( m_kCurrentVOI != null )
        {
            if ( m_kCurrentVOI.nearPoint( iX, iY, m_iSlice ) )
            {
                m_kParent.setCursor(MipavUtil.crosshairCursor);
                m_iNearStatus = NearPoint;
                return;
            }
            else if ( m_kCurrentVOI.nearLine( iX, iY, m_iSlice ) )
            {
                if ( (m_kCurrentVOI.getType() != VOI.POINT_3D) && 
                        (m_kCurrentVOI.getType() != VOI.LINE_3D) &&
                        (m_kCurrentVOI.getType() != VOI.POLYLINE_SLICE_3D) &&
                        (m_kCurrentVOI.getType() != VOI.PROTRACTOR_3D) &&
                        (m_kCurrentVOI.getType() != VOI.ANNOTATION_3D) )
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

        VOIVector kVOIs = m_kImageActive.get3DVOIs();
        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kVOI = kVOIs.get(i);
            for ( int j = 0; j < kVOI.getCurves()[0].size(); j++ )
            {
                LocalVolumeVOI kVOI3D = ((LocalVolumeVOI)kVOI.getCurves()[0].get(j));
                if ( kVOI3D.contains( m_iPlaneOrientation, iX, iY, m_iSlice ) )
                {
                    m_iNearStatus = NearNone;
                    m_kParent.setCursor(MipavUtil.moveCursor);
                    return;
                }
            }
        }
        m_iNearStatus = NearNone;
        m_kParent.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }

    /**
     * Creates a single level set. Takes a starting point and finds a closed path along the levelset back to the
     * starting point.
     *
     * @param  startPtX  the start point
     * @param  startPtY  the start point
     * @param  level     the level of the level set
     */
    private LocalVolumeVOI singleLevelSet(float startPtX, float startPtY ) {
        double distance;
        stack.removeAllElements();

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
        int x = (int) ( kPatientPt.X + 0.5);
        int y = (int) ( kPatientPt.Y + 0.5);
        int z = (int)kPatientPt.Z;

        setIndices( x, y, z );

        int index = m_aiIndexValues[m_i_][m_i_];
        int level = (m_aucBufferActive[index] & 0x00ff);

        levelSetStack.reset();
        levelSetStack.addPoint(x, y);
        map.set(m_aiIndexValues[m_i_][m_i_]);

        int dir = -1;
        float diff = 100000;
        int mapIndex = 0;

        do {

            if ((x >= 2) && (x < (m_aiLocalImageExtents[0] - 2)) && (y >= 2) && (y < (m_aiLocalImageExtents[1] - 2))) {

                mapIndex = m_aiIndexValues[m_iM][m_i_];
                if ((m_afAverages[m_iM][m_i_] >= level) &&
                        ((m_afAverages[m_iM][m_iP] < level) || (m_afAverages[m_i_][m_i_] < level) ||
                                (m_afAverages[m_iM][m_iM] < level) || (m_afAverages[m_iMM][m_i_] < level)) &&
                                (map.get(mapIndex) == false)) {
                    dir = 1;
                    diff = Math.abs(m_afAverages[m_iM][m_i_] - m_afAverages[m_i_][m_i_]);
                }
                mapIndex = m_aiIndexValues[m_iM][m_iP];
                if ((m_afAverages[m_iM][m_iP] >= level) &&
                        ((m_afAverages[m_iM][m_iPP] < level) || (m_afAverages[m_i_][m_iP] < level) ||
                                (m_afAverages[m_iM][m_i_] < level) || (m_afAverages[m_iMM][m_iP] < level)) &&
                                (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_iM][m_iP] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 2;
                        diff = Math.abs(m_afAverages[m_iM][m_iP] - m_afAverages[m_i_][m_i_]);
                    }
                }
                mapIndex = m_aiIndexValues[m_i_][m_iP];
                if ((m_afAverages[m_i_][m_iP] >= level) &&
                        ((m_afAverages[m_i_][m_iPP] < level) || (m_afAverages[m_iP][m_iP] < level) || (m_afAverages[m_i_][m_i_] < level) ||
                                (m_afAverages[m_iM][m_iP] < level)) && (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_i_][m_iP] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 3;
                        diff = Math.abs(m_afAverages[m_i_][m_iP] - m_afAverages[m_i_][m_i_]);
                    }
                }
                mapIndex = m_aiIndexValues[m_iP][m_iP];
                if ((m_afAverages[m_iP][m_iP] >= level) &&
                        ((m_afAverages[m_iP][m_iPP] < level) || (m_afAverages[m_iPP][m_iP] < level) ||
                                (m_afAverages[m_i_][m_iP] < level) || (m_afAverages[m_iP][m_i_] < level)) &&
                                (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_iP][m_iP] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 4;
                        diff = Math.abs(m_afAverages[m_iP][m_iP] - m_afAverages[m_i_][m_i_]);
                    }
                }
                mapIndex = m_aiIndexValues[m_iP][m_i_];
                if ((m_afAverages[m_iP][m_i_] >= level) &&
                        ((m_afAverages[m_iP][m_iP] < level) || (m_afAverages[m_iPP][m_i_] < level) ||
                                (m_afAverages[m_iP][m_iM] < level) || (m_afAverages[m_i_][m_i_] < level)) &&
                                (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_iP][m_i_] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 5;
                        diff = Math.abs(m_afAverages[m_iP][m_i_] - m_afAverages[m_i_][m_i_]);
                    }
                }
                mapIndex = m_aiIndexValues[m_iP][m_iM];
                if ((m_afAverages[m_iP][m_iM] >= level) &&
                        ((m_afAverages[m_iP][m_i_] < level) || (m_afAverages[m_iPP][m_iM] < level) ||
                                (m_afAverages[m_iP][m_iMM] < level) || (m_afAverages[m_i_][m_iM] < level)) &&
                                (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_iP][m_iM] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 6;
                        diff = Math.abs(m_afAverages[m_iP][m_iM] - m_afAverages[m_i_][m_i_]);
                    }
                }
                mapIndex = m_aiIndexValues[m_i_][m_iM];
                if ((m_afAverages[m_i_][m_iM] >= level) &&
                        ((m_afAverages[m_i_][m_i_] < level) || (m_afAverages[m_iP][m_iM] < level) || (m_afAverages[m_i_][m_iMM] < level) ||
                                (m_afAverages[m_iM][m_iM] < level)) && (map.get(mapIndex) == false)) {

                    if (Math.abs(m_afAverages[m_i_][m_iM] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 7;
                        diff = Math.abs(m_afAverages[m_i_][m_iM] - m_afAverages[m_i_][m_i_]);
                    }
                }
                mapIndex = m_aiIndexValues[m_iM][m_iM];
                if ((m_afAverages[m_iM][m_iM] >= level) &&
                        ((m_afAverages[m_iM][m_i_] < level) || (m_afAverages[m_i_][m_iM] < level) ||
                                (m_afAverages[m_iM][m_iMM] < level) || (m_afAverages[m_iMM][m_iM] < level)) &&
                                (map.get(mapIndex) == false))  {

                    if (Math.abs(m_afAverages[m_iM][m_iM] - m_afAverages[m_i_][m_i_]) < diff) {
                        dir = 0;
                        // diff = Math.abs(imageBufferActive[index-xDim-1] - imageBufferActive[index]);
                    }
                }

                diff = 1000000;

                if (dir == 1) {          
                    mapIndex = m_aiIndexValues[m_iM][m_i_];
                    map.set(mapIndex);
                    paths(x,y,z, dir, level);
                    // x = x;
                    y = y - 1;     
                    setIndices( x, y, z );
                } else if (dir == 2) {
                    mapIndex = m_aiIndexValues[m_iM][m_iP];
                    map.set(mapIndex);
                    paths(x,y,z, dir, level);
                    x = x + 1;
                    y = y - 1;               
                    setIndices( x, y, z );
                } else if (dir == 3) {
                    mapIndex = m_aiIndexValues[m_i_][m_iP];
                    map.set(mapIndex);
                    paths(x,y,z, dir, level);
                    x = x + 1;
                    // y = y;               
                    setIndices( x, y, z );
                } else if (dir == 4) {
                    mapIndex = m_aiIndexValues[m_iP][m_iP];
                    map.set(mapIndex);
                    paths(x,y,z, dir, level);
                    x = x + 1;
                    y = y + 1;               
                    setIndices( x, y, z );
                } else if (dir == 5) {
                    mapIndex = m_aiIndexValues[m_iP][m_i_];
                    map.set(mapIndex);
                    paths(x,y,z, dir, level);
                    // x = x;
                    y = y + 1;               
                    setIndices( x, y, z );
                } else if (dir == 6) {
                    mapIndex = m_aiIndexValues[m_iP][m_iM];
                    map.set(mapIndex);
                    paths(x,y,z, dir, level);
                    x = x - 1;
                    y = y + 1;               
                    setIndices( x, y, z );
                } else if (dir == 7) {
                    mapIndex = m_aiIndexValues[m_i_][m_iM];
                    map.set(mapIndex);
                    paths(x,y,z, dir, level);
                    x = x - 1;
                    // y = y;               
                    setIndices( x, y, z );
                } else if (dir == 0) {
                    mapIndex = m_aiIndexValues[m_iM][m_iM];
                    map.set(mapIndex);
                    paths(x,y,z, dir, level);
                    x = x - 1;
                    y = y - 1;               
                    setIndices( x, y, z );
                } else {

                    if (!stack.empty()) {
                        int ptr = stack.pop()[0];
                        x = levelSetStack.getPointX(ptr);
                        y = levelSetStack.getPointY(ptr);
                        levelSetStack.setIndex(ptr);
                        setIndices( x, y, z );
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
            distance = ((x - startPtX) * (x - startPtX)) + ((y - startPtY) * (y - startPtY));if ((distance < 2.1) && (levelSetStack.size() < 10)) {
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
                kPositions.add( kScreenPt );
            }
            kPatientPt.Set( levelSetStack.getPointX(0), levelSetStack.getPointY(0), m_iSlice );   
            kScreenPt = m_kDrawingContext.patientToScreen( kPatientPt );
            kPositions.add( kScreenPt );
            return new VOIContour3D(this, m_kDrawingContext, m_iPlaneOrientation, m_iDrawType, m_iDrawType, kPositions, false);
        } 
        return null;
    }
    
    
    
    


    /**
     * This method calculates the average pixel value based on the four neighbors (N, S, E, W).
     *
     * @param   index  the center pixel where the average pixel value is to be calculated.
     *
     * @return  the average pixel value as a float.
     */
    private float avgPix(int index) {
        int xDim = m_kLocalImage.getExtents()[0];

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

        intPtr[0] = levelSetStack.size() - 1;
        int xDim = m_kLocalImage.getExtents()[0];

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
     */
    private LocalVolumeVOI singleLevelSet(float startPtX, float startPtY, float level) {

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
        
        index = (y * xDim) + x;
        level = (m_aucBufferActive[index] & 0x00ff);

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
            return new VOIContour3D(this, m_kDrawingContext, m_iPlaneOrientation, m_iDrawType, m_iDrawType, kPositions, false);
        } 
        return null;
    }
    
    
    

    private void splitVOIs( boolean bAllSlices, boolean bOnlyActive, LocalVolumeVOI kSplitVOI )
    {
        VOIVector kVOIs = m_kImageActive.get3DVOIs();
        if ( (kSplitVOI.getSType() != SPLITLINE) || (kVOIs.size() == 0) )
        {
            return;
        }
        Vector<LocalVolumeVOI> kNewVOIs = new Vector<LocalVolumeVOI>();

        Vector3f kStartPt = fileCoordinatesToPatient(kSplitVOI.get(0)); 
        Vector3f kEndPt = fileCoordinatesToPatient(kSplitVOI.get(1)); 

        int iStartSlice = bAllSlices? 0 : m_iSlice;
        int iEndSlice = bAllSlices? m_aiLocalImageExtents[2] : m_iSlice + 1;


        for ( int i = 0; i < kVOIs.size(); i++ )
        {
            VOI kVOI = kVOIs.get(i);
            for ( int j = 0; j < kVOI.getCurves()[0].size(); j++ )
            {
                LocalVolumeVOI kVOI3D = ((LocalVolumeVOI)kVOI.getCurves()[0].get(j));
                if ( kVOI3D.getOrientation() != m_iPlaneOrientation )
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
                LocalVolumeVOI kNew = kVOI3D.split( kStartPt, kEndPt );
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
}
