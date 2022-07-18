package gov.nih.mipav.view.renderer.WildMagic.flythroughview;


import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphCurve;

import java.awt.Toolkit;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import WildMagic.LibFoundation.Curves.Curve3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Behavior which allows for flying down a specified path and looking around.
 */
public class FlyPathBehavior_WM implements KeyListener {

    /**
     * Instances which want to be notified of updates to this behavior should implement this interface by providing the
     * viewChanged method implementation.
     */
    public static interface Callback {

        /**
         * ViewChanged callback for flythrough renderers.
         * @param  kBehavior  reference to this MjFlyPathBehavior in which the view changed.
         * @param  iEvent     Bitmask identifies the event(s) which caused the view to change. Bitmask created from OR
         *                    of EVENT_* defintions.
         */
        void viewChanged(FlyPathBehavior_WM kBehavior, int iEvent);
    }

    /**
     * Branch information.
     */
    private class BranchState extends Object implements Cloneable {

        /** List of branch points (normalized path distances where one or more branches starts). */
        public float[] m_afBranchPoint;

        /** Current direction. True if forward, false if reverse. */
        public boolean m_bMoveForward;

        /** Current position along the path. */
        public float m_fNormalizedPathDist;

        /** Normalized distance (in range [0,1]) along parent branch where the specified 
         * branch starts. */
        public float m_fParentBranchPoint;

        /** Identifies the current curve in the path graph. */
        public int m_iBranchIndex;

        /** Information about the parent branch, if one exists. */
        public int m_iParentBranchIndex;

        /** Branch curve */
        public Curve3f m_kBranchCurve;

        /** Range of normalized path distances that have not been visited. */
        protected float m_fDistUnvisitedMax;

        /** Range of normalized path distances that have not been visited. */
        protected float m_fDistUnvisitedMin;

        /**
         * Constructor.
         *
         * @param  iBranchIndex   int Index which identifies the branch.
         * @param  kFlyPathGraph  FlyPathGraphCurve Data structure which contains all of the information about each
         *                        branch and its connections.
         */
        public BranchState(int iBranchIndex, FlyPathGraphCurve kFlyPathGraph) {
            m_iBranchIndex = iBranchIndex;
            m_kBranchCurve = kFlyPathGraph.getCurvePosition(iBranchIndex);

            m_fDistUnvisitedMin = 0.0f;
            m_fDistUnvisitedMax = 1.0f;

            m_afBranchPoint = kFlyPathGraph.getBranchPoints(iBranchIndex);
            m_iParentBranchIndex = kFlyPathGraph.getBranchParentIndex(iBranchIndex);
            m_fParentBranchPoint = kFlyPathGraph.getBranchParentNormalizedDist(iBranchIndex);

            start();
        }

        /**
         * Create a copy of this instance.
         * @return  BranchState New instance which is a copy of this instance.
         */
        public BranchState createCopy() {
            return (BranchState) clone();
        }

        /**
         * Create a copy of this instance which has the same information except that the state of the moving forward is
         * inverted.
         * @return  BranchState New instance which is a copy of this instance except that the moving forward flag is
         *          inverted.
         */
        public BranchState createReverseCopy() {
            BranchState kCopy = createCopy();

            kCopy.m_bMoveForward = !kCopy.m_bMoveForward;

            return kCopy;
        }

        /**
         * Return an index which identifies segment the specified normalized path distance belongs.
         *
         * @param   fNormalizedPathDist  float Normalized path distance in the range [0,1] along the branch path.
         *
         * @return  int An index of zero is returned if the distance is before the first branch point or if there are no
         *          branch points. An index of one is returned if the distance is greater than or equal to the first
         *          branch point but less than or equal to second branch point.
         */
        public int getBranchPointSegment(float fNormalizedPathDist) {
            int iSegment = 0;

            while (iSegment < m_afBranchPoint.length) {

                // When moving forward, the branch point counts as being
                // in the next segment if equal to the input distance.
                if (m_bMoveForward) {

                    if (fNormalizedPathDist < m_afBranchPoint[iSegment]) {
                        break;
                    }
                } // When moving backward, the branch point counts as being

                // in the previous segment if equal to the input distance.
                else {

                    if (fNormalizedPathDist <= m_afBranchPoint[iSegment]) {
                        break;
                    }
                }

                ++iSegment;
            }

            return iSegment;
        }

        /**
         * Return the position of the curve of the point further down the curve the specified distance in the current
         * heading.
         *
         * @param   fDist  float Distance further down the branch curve in the current heading. This value can be
         *                 negative for a point in the reverse heading.
         *
         * @return  Point3f Coordinates of the 3D point further down along the curve.
         */
        public Vector3f getForwardNormalizedPosition(float fDist) {
        	return m_kBranchCurve.GetPosition(getForwardNormalizedTime(fDist));
        }

        /**
         * Return the normalized path distance of a point further down the branch curve in the current heading.
         *
         * @param   fForwardDist  float Distance further down the branch curve in the current heading. This value can be
         *                        negative for a point in the reverse heading.
         *
         * @return  float Normalized path distance in the [0,1] range for the requested point.
         */
        public float getForwardNormalizedTime(float fForwardDist) {

            // Normalize the input distance.
            float fPathDist = m_fNormalizedPathDist * m_kBranchCurve.GetTotalLength();

            return m_bMoveForward ? m_kBranchCurve.GetTime(fPathDist + fForwardDist, 100, 1e-02f)
                                  : m_kBranchCurve.GetTime(fPathDist - fForwardDist, 100, 1e-02f);

        }

        /**
         * Reset parameters to start at the beginning of the path moving in the forward direction.
         */
        public void start() {
            m_fNormalizedPathDist = 0.0f;
            m_bMoveForward = true;
        }

        /**
         * Update the range of normalized path distances that have not been visited. Call this method before changing
         * the current normalized path distance!
         *
         * @param  fNewNormalizedPathDistance  float Normalized path distance about to be set for this branch.
         */
        public void updateDistUnvisited(float fNewNormalizedPathDistance) {

            if (m_fNormalizedPathDist <= m_fDistUnvisitedMin) {
                m_fDistUnvisitedMin = Math.max(m_fDistUnvisitedMin, fNewNormalizedPathDistance);
            }

            if (m_fNormalizedPathDist >= m_fDistUnvisitedMax) {
                m_fDistUnvisitedMax = Math.min(m_fDistUnvisitedMax, fNewNormalizedPathDistance);
            }
        }

        /**
         * Clone the current branch state. Used by the mouse recording process.
         *
         * @return  Object
         *
         * @throws  InternalError  DOCUMENT ME!
         */
        @Override
		protected Object clone() {

            // Should not get clone unsupported exception since we use
            // the Object mehtod's clone which just replicated data
            // values and references.
            try {
                return super.clone();
            } catch (CloneNotSupportedException e) {
                throw new InternalError(e.toString());
            }
        }
    }

    /** on a viewChanged event change both the view position and direction vector */
    public static final int EVENT_CHANGE_ALL = 0xffffffff;
    
    /** on a viewChanged event change just the view position  */
    public static final int EVENT_CHANGE_POSITION = 0x00000001;

    /** on a viewChanged event change just the view direction */
    public static final int EVENT_CHANGE_VIEW = 0x00000002;

    /** on a viewChanged event change just the view orientation */
    public static final int EVENT_RESET_ORIENTATION = 0x00000004;

    /** on a viewChanged event change the fly path branch */
    public static final int EVENT_CHANGE_BRANCH = 0x00000008;

    /**
     * Sound a beep.
     */
    private static void beep() {
        Toolkit.getDefaultToolkit().beep();
    }

    /** current and previous key press time. */
    long currEventTime, prevEventTime;

    /**
     * How far ahead down the path should the view be aimed. If zero, then the view direction is the tangent to the
     * curve. If positive, the the view direction is to look at that point along the path.
     */
    public float m_fGazeDist;

    /**
     * What is the increment in distance along the path that each step takes. Increment may be negative indicating that
     * the path is being followed in the opposite direction.
     */
    public float m_fPathStep;

    /**
     * When a branch point along the current branch is reached, this array is filled with the possible BranchState
     * instances that the user can select from for following. Once movement is made along one of those selected from the
     * list, then this list is cleared. An index is maintained to indicate which branch in the list is currently
     * selected.
     */
    private BranchState[] m_akBranchChoice = null;

    /** Identifies the current state of traversing the current branch. */
    private BranchState[] m_akBranchState;

    /**
     * When a branch point along the current branch is reached, this flag is set until the user selects a branch to
     * follow.
     */
    private boolean m_bChooseBranch = false;

    /** Current annotation in the list. */
    private int m_iAnnotateListItemSelected = -1;

    /** Index of the closest branch at a fork in the path. */
    private int m_iBranchChoiceIndex = -1;

    /** Keep reference to instance which describes the annotation points. */
    private FlyPathAnnotateList_WM m_kAnnotateList;

    /** Current branch state of traversing the fly path. */
    private BranchState m_kBranchState = null;

    /**
     * Instance which implements the Callback interface whose viewChanged method is to be called whenever anything about
     * this behavior changes.
     */
    private Callback m_kCallback = null;

    /** Keep reference to instance which describes the path. */
    private FlyPathGraphCurve m_kFlyPathGraph;
    /**
     * The desired view up vector is a normalized average of these two orthogonal axes vectors. The problem is that if
     * just one desired view up vector is chosen, then when the view direction vector is "aligned" with that vector,
     * some other view up vector would have to be chosen and some rule would have to be defined for that.
     */
    private Vector3f m_kViewup1 = new Vector3f(0.0f, 1.0f, 0.0f);
    /** For calculating the view up vector. */
    private Vector3f m_kViewup2 = new Vector3f(0.0f, 0.0f, 1.0f);
    
    /** Parent frame references. */
    private FlyThroughRender parentScene;

    /** Current view position along the path. */
    private Vector3f m_kViewPoint = new Vector3f(0f,0f,0f);

    /** Current view direction along the path. */
    private Vector3f m_kViewDirection = new Vector3f(0f,0f,0f);

    /** Current view up vector. */
    private Vector3f m_kViewUp = new Vector3f(0f,0f,0f);

    /**
     * Setup to fly along the specified path and look around.
     *
     * @param  kFlyPathGraph          FlyPathGraphCurve contains the information regarding the graph representation of
     *                                3D path segments represented by Curve3 instances
     * @param  kAnnotateList          FlyPathAnnotateList contains the list of annotation points.
     * @param  kTransformPosition     TransformGroup contains the Transform3D for the current viewing position.
     * @param  kTransformDirection    TransformGroup contains the Transform3D for the current viewing direction.
     * @param  kTransformOrientation  TransformGroup contains the Transform3D for the current viewing orientation.
     * @param  _parentScene           the parent frame which hold Canvas3D.
     */
    public FlyPathBehavior_WM(FlyPathGraphCurve kFlyPathGraph, FlyPathAnnotateList_WM kAnnotateList,FlyThroughRender _parentScene) {

        // Keep references to these.
        m_kFlyPathGraph = kFlyPathGraph;
        m_kAnnotateList = kAnnotateList;
        parentScene = _parentScene;
        parentScene.GetCanvas().addKeyListener(this);

        // Create array to store the state of each branch.
        int iNumBranches = kFlyPathGraph.getNumBranches();

        m_akBranchState = new BranchState[iNumBranches];

        for (int iBranch = 0; iBranch < iNumBranches; iBranch++) {
            m_akBranchState[iBranch] = new BranchState(iBranch, kFlyPathGraph);
        }

        // Set initial branch, position, and step.
        // Compute the distance increment along the path that each
        // step will take.
        m_fPathStep = 1.0f;
        m_fGazeDist = 10.0f;
        setBranch(0);
    }

    /**
     * One round trip path walk through.
     */
    public void autoRun() {
        int i, j;
        float steps = getPathLength() / getPathStep();

        if (m_akBranchState != null) {

            for (i = 0; i < m_akBranchState.length; i++) {
                setBranch(i);

                for (j = 0; j < steps; j++) {
                    moveAlongPath(1);   
                }

                for (j = 0; j < steps; j++) {
                    moveAlongPath(-1);
                }
            }
        }

        setBranch(0);

        if (null != m_akBranchChoice) {
            setClosestChoiceBranch();
        } else {
            beep();
        }

    }

    /**
     * Access the curve in the path graph currently positioned along.
     * @return  Curve3 Reference to Curve3 instance in path graph.
     */
    public Curve3f getBranchCurve() {
        return m_kBranchState.m_kBranchCurve;
    }

    /**
     * Access the normalized distance traveled from the end of the current branch path. Also represents the maximum of
     * the range that has been unvisited.
     * @return  float Normalized distance along the current branch path that is maximum of the unvisited range.
     */
    public float getBranchDistUnvisitedMax() {
        return m_kBranchState.m_fDistUnvisitedMax;
    }

    /**
     * Access the normalized distance traveled from the beginning of the current branch path. Also represents the
     * minimum of the range that has been unvisited.
     * @return  float Normalized distance along the current branch path that is minimum of the unvisited range.
     */
    public float getBranchDistUnvisitedMin() {
        return m_kBranchState.m_fDistUnvisitedMin;
    }

    /**
     * Access the index of the curve in the path graph currrently positioned along.
     * @return  int Index of Curve3 instance in path graph.
     */
    public int getBranchIndex() {
        return m_kBranchState.m_iBranchIndex;
    }

    /**
     * Get the current state of traversing.
     * @return  BranchState
     */
    public BranchState getBranchState() {
        return m_kBranchState.createCopy();
    }

    /**
     * Get the current distance ahead for looking down the path. If this distance is zero, then the view direction is
     * the tangent to the path curve at the current position.
     * @return  float Distance ahead for looking down the path.
     */
    public float getGazeDistance() {
        return m_fGazeDist;
    }

    /**
     * Get the normalized distance along the current path.
     * @return  float Value in the range [0,1].
     */
    public float getNormalizedPathDistance() {
        return m_kBranchState.m_fNormalizedPathDist;
    }
    
    /**
     * Get the current position distance along the path.
     * @return  float path distance.
     */
    public float getPathDist() {
        return m_kBranchState.m_fNormalizedPathDist;
    }

    /**
     * Get the current distance along the path.
     * @return  distance along the path.
     */
    public float getPathDistance() {
        return getNormalizedPathDistance() * getPathLength();
    }


    /**
     * Get the total length of the current path.
     * @return  float Length of the path.
     */
    public float getPathLength() {
        return m_kBranchState.m_kBranchCurve.GetTotalLength();
    }

    /**
     * Get the current distance increment for moving along the path.
     * @return  distance increment for moving along path. Always positive regardless of which direction moving along the
     *          path.
     */
    public float getPathStep() {
        return m_fPathStep;
    }

    /**
     * Access the index of the currently selected branch at a branch point.
     * @return  int Index of currently selected branch; -1 if not currently at a branch point.
     */
    public int getSelectedBranchIndex() {

        if ((null != m_akBranchChoice) && (-1 != m_iBranchChoiceIndex)) {
            return m_akBranchChoice[m_iBranchChoiceIndex].m_iBranchIndex;
        }

        return -1;
    }

    /**
     * Returns the view direction vector at the current position on the path.
     * @return view direction vector at the current position on the path.
     */
    public Vector3f getViewDirection()
    {
        return m_kViewDirection;
    }
    /**
     * Returns the current position on the path.
     * @return current position on the path.
     */
    public Vector3f getViewPoint()
    {
        return m_kViewPoint;
    }
    /**
     * Returns the view up vector at the current position on the path.
     * @return view up vector at the current position on the path.
     */
    public Vector3f getViewUp()
    {
        return m_kViewUp;
    }

    /**
     * Get indication of whether the movement along the current path is in the forward or reverse direction.
     * @return  boolean True if the movement is along the forward direction along the path.
     */
    public boolean isPathMoveForward() {
        return m_kBranchState.m_bMoveForward;
    }

    /**
     * Handle the key pressed event from the text field.
     * @param  event  key event to handle
     */
    @Override
	public void keyPressed(KeyEvent event) {
        if (KeyEvent.KEY_PRESSED == event.getID()) {
            int iKeyCode = event.getKeyCode();
            char iKeyChar = event.getKeyChar();

            switch (iKeyCode) {

            case KeyEvent.VK_ESCAPE:
                setIdentityViewOrientation();
                break;

                case KeyEvent.VK_HOME:

                    // reset position to the beginning of the path
                    if (!m_bChooseBranch && (null == m_akBranchChoice)) {
                        setPathDist(0.0f);
                    } else {
                        beep();
                    }

                    break;

                case KeyEvent.VK_END:

                    // reset position to the end of the path
                    if (!m_bChooseBranch && (null == m_akBranchChoice)) {
                        setPathDist(1.0f);
                    } else {
                        beep();
                    }

                    break;

                case KeyEvent.VK_UP:

                    // move forward along the path
                    if (!m_bChooseBranch) {
                        doPathStep(1);
                    } else {
                        beep();
                    }

                    break;

                case KeyEvent.VK_DOWN:

                    // move backward along the path
                    if (!m_bChooseBranch) {
                        doPathStep(-1);
                    } else {
                        beep();
                    }

                    break;

                case KeyEvent.VK_R:

                    // follow path in reverse heading
                    m_kBranchState.m_bMoveForward = !m_kBranchState.m_bMoveForward;
                    setPathDist(m_kBranchState.m_fNormalizedPathDist);
                    break;

                case KeyEvent.VK_F5:

                    // go to previous annotate point
                    if (!m_bChooseBranch && (m_kAnnotateList.getNumItems() > 0)) {

                        if (--m_iAnnotateListItemSelected < 0) {
                            m_iAnnotateListItemSelected = m_kAnnotateList.getNumItems() - 1;
                        }

                        setCurvePathAnnotateItem(m_iAnnotateListItemSelected);
                    } else {
                        beep();
                    }

                    break;

                case KeyEvent.VK_F6:

                    // go to next annotate point
                    if (!m_bChooseBranch && (m_kAnnotateList.getNumItems() > 0)) {

                        if (++m_iAnnotateListItemSelected >= m_kAnnotateList.getNumItems()) {
                            m_iAnnotateListItemSelected = 0;
                        }

                        setCurvePathAnnotateItem(m_iAnnotateListItemSelected);
                    } else {
                        beep();
                    }

                    break;

                case KeyEvent.VK_SPACE:

                    // select next branch choice
                    if (null != m_akBranchChoice) {
                        setClosestChoiceBranch();
                    } else {
                        beep();
                    }

                    break;

                case KeyEvent.VK_S:

                    // change the distance of a single step
                    if ('s' == iKeyChar) {
                        m_fPathStep -= 0.1f;

                        if (m_fPathStep < 0.1f) {
                            m_fPathStep = 0.1f;
                            beep();
                        }
                    } else {
                        m_fPathStep += 0.1f;
                    }

                    setPathDist(m_kBranchState.m_fNormalizedPathDist);
                    break;

                case KeyEvent.VK_G:

                    // change the gaze distance
                    if ('g' == iKeyChar) {
                        m_fGazeDist -= 1.0f;

                        if (m_fGazeDist < 0.0f) {
                            m_fGazeDist = 0.0f;
                            beep();
                        }
                    } else {
                        m_fGazeDist += 1.0f;
                    }

                    setPathDist(m_kBranchState.m_fNormalizedPathDist);
                    break;
            }
        }
    }

    /* (non-Javadoc)
     * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
     */
    @Override
	public void keyReleased(KeyEvent event) {}

    /* (non-Javadoc)
     * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
     */
    @Override
	public void keyTyped(KeyEvent event) {}

    /**
     * Call from the JPanelFlythruMove.
     * @param  command  move command.
     */
    public void move(String command) {

        if (command.equals("lookup")) {
            // pitch - look up
            Vector3f kRight = Vector3f.unitCross( m_kViewDirection, m_kViewUp );
            Matrix3f kRotate = new Matrix3f();
            kRotate.fromAxisAngle( kRight, (float)Math.toRadians(1) );
            kRotate.mult( m_kViewDirection, m_kViewDirection );
            kRotate.mult( m_kViewUp, m_kViewUp );
            // Notify listener that we are updated.
            notifyCallback(EVENT_CHANGE_POSITION);
        } else if (command.equals("lookdown")) {
            // pitch - look down
            Vector3f kRight = Vector3f.unitCross( m_kViewDirection, m_kViewUp );
            Matrix3f kRotate = new Matrix3f();
            kRotate.fromAxisAngle( kRight, (float)Math.toRadians(-1) );
            kRotate.mult( m_kViewDirection, m_kViewDirection );
            kRotate.mult( m_kViewUp, m_kViewUp );
            // Notify listener that we are updated.
            notifyCallback(EVENT_CHANGE_POSITION);
        } else if (command.equals("lookleft")) {
            // yaw - look left
            Matrix3f kRotate = new Matrix3f();
            kRotate.fromAxisAngle( m_kViewUp, (float)Math.toRadians(1) );
            kRotate.mult( m_kViewDirection, m_kViewDirection );
            // Notify listener that we are updated.
            notifyCallback(EVENT_CHANGE_POSITION);
        } else if (command.equals("lookright")) {
            // case KeyEvent.VK_RIGHT:
            // yaw - look right
            Matrix3f kRotate = new Matrix3f();
            kRotate.fromAxisAngle( m_kViewUp, (float)Math.toRadians(-1) );
            kRotate.mult( m_kViewDirection, m_kViewDirection );
            // Notify listener that we are updated.
            notifyCallback(EVENT_CHANGE_POSITION);
        } else if (command.equals("counterclockwise")) {
            // case KeyEvent.VK_F3:
            // roll - counterclockwise
            Matrix3f kRotate = new Matrix3f();
            kRotate.fromAxisAngle( m_kViewDirection, (float)Math.toRadians(-1) );
            kRotate.mult( m_kViewUp,  m_kViewUp );
            // Notify listener that we are updated.
            notifyCallback(EVENT_CHANGE_POSITION);
        } else if (command.equals("clockwise")) {
            // roll - clockwise
            Matrix3f kRotate = new Matrix3f();
            kRotate.fromAxisAngle( m_kViewDirection, (float)Math.toRadians(1) );
            kRotate.mult( m_kViewUp,  m_kViewUp );
            // Notify listener that we are updated.
            notifyCallback(EVENT_CHANGE_POSITION);
        } else if (command.equals("escape")) {

            // VK_ESCAPE
            setIdentityViewOrientation();
        } else if (command.equals("home")) {

            // case KeyEvent.VK_HOME:
            // reset position to the beginning of the path
            if (!m_bChooseBranch && (null == m_akBranchChoice)) {
                setPathDist(0.0f);
            } else {
                beep();
            }
        } else if (command.equals("end")) {

            // case KeyEvent.VK_END:
            // reset position to the end of the path
            if (!m_bChooseBranch && (null == m_akBranchChoice)) {
                setPathDist(1.0f);
            } else {
                beep();
            }
        } else if (command.equals("forward")) {

            // case KeyEvent.VK_UP:
            // move forward along the path
            if (!m_bChooseBranch) {
                doPathStep(1);
            } else {
                beep();
            }
        } else if (command.equals("backward")) {

            // case KeyEvent.VK_DOWN:
            // move backward along the path
            if (!m_bChooseBranch) {
                doPathStep(-1);
            } else {
                beep();
            }
        } else if (command.equals("reverse")) {

            // case KeyEvent.VK_R:
            // follow path in reverse heading
            m_kBranchState.m_bMoveForward = !m_kBranchState.m_bMoveForward;
            setPathDist(m_kBranchState.m_fNormalizedPathDist);

        } else if (command.equals("prevAnnotatePt")) {

            // case KeyEvent.VK_F5:
            // go to previous annotate point
            if (!m_bChooseBranch && (m_kAnnotateList.getNumItems() > 0)) {

                if (--m_iAnnotateListItemSelected < 0) {
                    m_iAnnotateListItemSelected = m_kAnnotateList.getNumItems() - 1;
                }

                setCurvePathAnnotateItem(m_iAnnotateListItemSelected);
            } else {
                beep();
            }
        } else if (command.equals("nextAnnotatePt")) {

            // case KeyEvent.VK_F6:
            // go to next annotate point
            if (!m_bChooseBranch && (m_kAnnotateList.getNumItems() > 0)) {

                if (++m_iAnnotateListItemSelected >= m_kAnnotateList.getNumItems()) {
                    m_iAnnotateListItemSelected = 0;
                }

                setCurvePathAnnotateItem(m_iAnnotateListItemSelected);
            } else {
                beep();
            }
        } else if (command.equals("nextBranch")) {

            // case KeyEvent.VK_SPACE:
            // select next branch choice
            if (null != m_akBranchChoice) {
                setClosestChoiceBranch();
            } else {
                beep();
            }
        } else if (command.equals("stepDistanceIncrease")) {
            m_fPathStep += 0.1f;
            setPathDist(m_kBranchState.m_fNormalizedPathDist);
        } else if (command.equals("stepDistanceDecrease")) {
            m_fPathStep -= 0.1f;

            if (m_fPathStep < 0.1f) {
                m_fPathStep = 0.1f;
                beep();
            }

            setPathDist(m_kBranchState.m_fNormalizedPathDist);
        } else if (command.equals("gazeDistanceDecrease")) {
            m_fGazeDist -= 1.0f;

            if (m_fGazeDist < 0.0f) {
                m_fGazeDist = 0.0f;
                beep();
            }

            setPathDist(m_kBranchState.m_fNormalizedPathDist);
        } else if (command.equals("gazeDistanceIncrease")) {
            m_fGazeDist += 1.0f;
            setPathDist(m_kBranchState.m_fNormalizedPathDist);
        }

    }

    /**
     * Move along the path step width.
      * @param  _step  step size.
     */
    public void moveAlongPath(int _step) {
        doPathStep(_step);
    }

    /**
     * Make the specified branch be the current state for following.
     * @param  _kBranchState  BranchState Instance which describes the state of the branch.
     */
    public void setBranch(Object _kBranchState) {
        m_kBranchState = ((BranchState) _kBranchState).createCopy();
        notifyCallback(EVENT_CHANGE_BRANCH);
        setPathDist(m_kBranchState.m_fNormalizedPathDist);
        setIdentityViewOrientation();
    }

    /**
     * Identify the instance which implements the Callback interface. The viewChanged method of the Callback interface
     * will be called whenever anything about this behavior changes.
     * @param  kCallback  reference to instance implementing the Callback interface; may pass null reference to disable
     *                    the callback
     */
    public void setupCallback(Callback kCallback) {
        m_kCallback = kCallback;
        notifyCallback(EVENT_CHANGE_ALL);
    }

    /**
     * Clamp the input normalized path distance to the range [0,1]. If the value is outside that range, then sound a
     * beep.
     * @param   fDistance  float Input normalized path distance.
     * @return  float Input distance clamped to the range [0,1].
     */
    private float clampNormalizedPathDistance(float fDistance) {

        // Clamp the distance to something along the path.
        if (fDistance > 1.0f) {
            beep();

            return 1.0f;
        } else if (fDistance < 0.0f) {
            beep();

            return 0.0f;
        }

        return fDistance;
    }

    /**
     * Take the specified number of steps along the current path using the current path step size. If the number of
     * steps is negative, then the steps are taken in reverse.
     * @param  iNumSteps  int Number of steps to take along the current path.
     */
    private void doPathStep(int iNumSteps) {

        // If we make a step and there were branch choices defined,
        // then replace the current state for the branch with the
        // variation selected.
        boolean bFirstSelectedBranchStep = false;

        if (null != m_akBranchChoice) {
            m_akBranchState[m_kBranchState.m_iBranchIndex] = m_kBranchState;
            m_akBranchChoice = null;
            notifyCallback(EVENT_CHANGE_BRANCH);
            bFirstSelectedBranchStep = true;
        }

        // Note that iNumSteps may be negative!
        float fNormalizedPathStep = iNumSteps * getPathStep() / getPathLength();

        // Reverse direction if moving backward.
        if (!m_kBranchState.m_bMoveForward) {
            fNormalizedPathStep = -fNormalizedPathStep;
        }

        // Compute the new normalized path distance.
        float fNewNormalizedPathDistance = getNormalizedPathDistance() + fNormalizedPathStep;

        // Note which branch we are currently on before we possibly change it.
        int iBranch = getBranchIndex();

        // Determine which segment of the branch path, in relation to the
        // branch points, that we are currently in and that we are
        // stepping into.
        int iPathSegment = m_kBranchState.getBranchPointSegment(getNormalizedPathDistance());
        int iNewPathSegment = m_kBranchState.getBranchPointSegment(fNewNormalizedPathDistance);

        // If the segments are different, then we will step into the branch.
        // The subbranch information is indexed by the minimum
        // of the two indexes.  Don't check if this is the first step
        // being taken after selecting a branch at a branch point.
        if (((Math.abs(iPathSegment - iNewPathSegment) >= 1) && !bFirstSelectedBranchStep) ||
                ((Math.abs(iPathSegment - iNewPathSegment) > 1) && bFirstSelectedBranchStep)) {
            beep();

            // Access the branch point by its segment.
            int iSegment = Math.min(iPathSegment, iNewPathSegment);

            // Clamp the path distance to this branch point.
            fNewNormalizedPathDistance = m_kBranchState.m_afBranchPoint[iSegment];
            m_kBranchState.updateDistUnvisited(fNewNormalizedPathDistance);
            setPathDist(fNewNormalizedPathDistance);

            // Build the list of possible branches.
            setupBranchChoices(iBranch, iSegment);
        } // Check for reaching the beginning of the branch in which

        // there is a parent.  Then we will step back onto the parent.
        // Don't check if this is the first step being taken after
        // selecting a branch at a branch point.
        else if ((fNewNormalizedPathDistance < 0.0f) && (-1 != m_kBranchState.m_iParentBranchIndex) &&
                     !bFirstSelectedBranchStep) {
            beep();

            // Clamp the path distance to this branch point.
            fNewNormalizedPathDistance = 0.0f;
            m_kBranchState.updateDistUnvisited(fNewNormalizedPathDistance);
            setPathDist(fNewNormalizedPathDistance);

            // Access the branch point by its segment.
            int iBranchParent = m_kBranchState.m_iParentBranchIndex;
            BranchState kBranchStateParent = m_akBranchState[iBranchParent];
            int iSegment = 0;

            while (iSegment < kBranchStateParent.m_afBranchPoint.length) {

                if (m_kBranchState.m_fParentBranchPoint == kBranchStateParent.m_afBranchPoint[iSegment]) {
                    break;
                }

                ++iSegment;
            }

            // Build the list of possible branches.
            setupBranchChoices(iBranchParent, iSegment);
        } // Remain on the same branch.
        else {

            // Make sure the distance is in the [0,1] range.
            fNewNormalizedPathDistance = clampNormalizedPathDistance(fNewNormalizedPathDistance);

            m_kBranchState.updateDistUnvisited(fNewNormalizedPathDistance);
            setPathDist(fNewNormalizedPathDistance);
        }
    }

    /**
     * Call the implementation of the callback's notification method, if a callback instance has been defined.
     * @param  iEvent  Bitmask identifies the event(s) which caused the view to change. Bitmask created from OR of
     *                 EVENT_* defintions.
     */
    private void notifyCallback(int iEvent) {

        if (null != m_kCallback) {
            m_kCallback.viewChanged(this, iEvent);
        }
    }
    
    /**
     * Make the specified branch be the current state for following.
     * @param  kBranchState  BranchState Instance which describes the state of the branch.
     */
    private void setBranch(BranchState kBranchState) {
        m_kBranchState = kBranchState;
        notifyCallback(EVENT_CHANGE_BRANCH);

        setPathDist(m_kBranchState.m_fNormalizedPathDist);
        setIdentityViewOrientation();
    }

    /**
     * Make the specified branch be the current state for following.
     * @param  iBranch  int Index of the branch path to follow.
     */
    private void setBranch(int iBranch) {
        setBranch(m_akBranchState[iBranch]);
    }

    /**
     * Loop through all of the possible branch choices and select the one that is the closest to the current view
     * direction vector. Take a vector from the current view point to a point along each choice branch. Compute the
     * dot-product between the current view direction vector and each of these branch direction vectors and take the one
     * with the largest positive value, i.e., most in alignment.
     */
    private void setClosestChoiceBranch() {
        // Retrieve the current combined viewing direction vector.
        // Note that the sign of the view direction vector is negated
        // for the reasons described in the setView method.
        //Matrix4f kMatrixView = parentScene.getWVMatrix();
        //Vector3f kViewDirection = new Vector3f(-kMatrixView.M02, -kMatrixView.M12, -kMatrixView.M22);
        Vector3f kViewDirection = new Vector3f(parentScene.getCameraDirection());

        // Record the current view position and combined view orientation.
        //Vector3f kP0 = new Vector3f(kMatrixView.M03, kMatrixView.M13, kMatrixView.M23);
        Vector3f kP0 = new Vector3f(getViewPoint());

        // Check point down path which is maximum of the step distance
        // and the gaze distance.
        float fPointDist = Math.max(m_fGazeDist, m_fPathStep);
        float fBestAlign = -1.0f;
        int iBestAlignBranchChoiceIndex = -1;

        for (int iBranch = 0; iBranch < m_akBranchChoice.length; iBranch++) {

            // Get vector from current view point to point down branch path.
            BranchState kBranch = m_akBranchChoice[iBranch];
            Vector3f kV = Vector3f.sub(kBranch.getForwardNormalizedPosition(fPointDist), kP0);
            kV.normalize();

            // Only accept the best aligned branches we can supposedly see.
            float fAlign = kV.dot(kViewDirection);

            if ((fAlign > 0.0f) && (fAlign > fBestAlign)) {
                fBestAlign = fAlign;
                iBestAlignBranchChoiceIndex = iBranch;
            }
        }

        // Select the "nearest" branch.
        if (iBestAlignBranchChoiceIndex >= 0) {

            // Select the new branch.
            m_iBranchChoiceIndex = iBestAlignBranchChoiceIndex;
            m_bChooseBranch = false;
            setBranch(m_akBranchChoice[m_iBranchChoiceIndex]);
        } else {
            beep();
        }
    }
    
    /**
     * Set the annotation point along the path.
     * @param  iItem  int
     */
    private void setCurvePathAnnotateItem(int iItem) {

        // Select the curve and the position along the curve.
        // First set the sign of the path step to reflect
        // whether the movement down the path was forward or backward
        // when the annotation point was captured.
        FlyPathAnnotateList_WM.Item kItem = m_kAnnotateList.getItem(iItem);

        m_kBranchState.m_bMoveForward = kItem.isPathMoveForward();
        setBranch(kItem.getBranchIndex());

        m_kViewPoint.copy(kItem.getCameraLocation());
        m_kViewDirection.copy(kItem.getCameraDirection());
        m_kViewUp.copy(kItem.getCameraUp());

        notifyCallback(EVENT_CHANGE_POSITION);

    }
    
    /**
     * Reset the view orientation transformation to the identity. That is, remove all yaw/pitch/roll.
     */
    private void setIdentityViewOrientation() {
        notifyCallback(EVENT_RESET_ORIENTATION);
    }
    
    /**
     * Update the camera position along the path based on the specified distance from the beginning.
     * @param  fNormalizedDist  normalized distance from the beginning of the path for the location of the camera for
     *                          viewing
     */
    private void setPathDist(float fNormalizedDist) {

        // Make sure the distance is in the [0,1] range.
        fNormalizedDist = clampNormalizedPathDistance(fNormalizedDist);

        // Compute the actual distance along the curve.
        float fDist = fNormalizedDist * getPathLength();

        // Save the current distance.
        m_kBranchState.m_fNormalizedPathDist = fNormalizedDist;

        // Get the path point (position and tangent) based on distance.
        // It needs to be double precision for the view to use.
        Curve3f kCurve = m_kBranchState.m_kBranchCurve;
        float fTime = kCurve.GetTime(fDist, 100, 1e-02f);
        Vector3f kViewPoint = kCurve.GetPosition(fTime);

        // If the gaze distance is zero, then use the tangent vector
        // to the curve.
        // If the path is being followed in the reverse direction,
        // then the direction of looking down the path needs to
        // be reversed (negated).
        Vector3f kLookatVector = new Vector3f();
        boolean bLookatVectorUseTangent = true;

        if (m_fGazeDist > 0.0f) {
            float fTimeGazeDist = m_kBranchState.getForwardNormalizedTime(m_fGazeDist);

            if (fTime != fTimeGazeDist) {
                Vector3f kVec = kCurve.GetPosition(fTimeGazeDist);
                kLookatVector = Vector3f.sub(kVec, kViewPoint);
                kLookatVector.normalize();
                bLookatVectorUseTangent = false;
            }
        }

        if (bLookatVectorUseTangent) {
            kLookatVector = kCurve.GetTangent(fTime);

            if (!m_kBranchState.m_bMoveForward) {
                kLookatVector.neg();
            }
        }

        // Update the view given the view position, view direction,
        // and a hint for the view up vector.
        setView(kViewPoint, kLookatVector);

        // Notify listener that we are updated.
        notifyCallback(EVENT_CHANGE_POSITION);
    }
    


    //~ Inner Interfaces -----------------------------------------------------------------------------------------------

    /**
     * Create the array of branch choices and set the mode which forces the user to select from among the branch
     * choices.
     * @param  iBranchParent  int Index which identifies the parent branch.
     * @param  iBranchPoint   int Index which identifies the point along the branch path where the branching occurs for
     *                        the choices.
     */
    private void setupBranchChoices(int iBranchParent, int iBranchPoint) {

        // Get list of possible sub-branches for the parent.
        int[] aiBranchChildIndex = m_kFlyPathGraph.getBranchPointBranches(iBranchParent, iBranchPoint);

        // Get the information for the parent branch.
        BranchState kBranchStateParent = m_akBranchState[iBranchParent];

        // Build the list of possible branches.
        // First branch choice is the parent branch, current direction
        // Last branch choice is the parent branch, reverse direction
        // Reset all of the sub-branches to their start state.
        m_akBranchChoice = new BranchState[2 + aiBranchChildIndex.length];
        m_akBranchChoice[0] = kBranchStateParent.createCopy();
        m_akBranchChoice[m_akBranchChoice.length - 1] = kBranchStateParent.createReverseCopy();

        for (int i = 0; i < aiBranchChildIndex.length; i++) {
            int iBranchChild = aiBranchChildIndex[i];

            m_akBranchState[iBranchChild].start();
            m_akBranchChoice[i + 1] = m_akBranchState[iBranchChild].createCopy();
        }

        m_bChooseBranch = true;
        m_iBranchChoiceIndex = -1;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------


    /**
     * Set the camera to the specified be located at the specified view point and looking in the specified direction.
     * @param  kViewPoint      coordinates of the camera view point
     * @param  kViewdirVector  coordinates of the camera view direction vector. This vector must be normalized.
     */
    private void setView(Vector3f kViewPoint, Vector3f kViewdirVector) {        
        // Use the view direction vector to create positive weights where more
        // weight is given to an axis that has less of a component in the
        // direction vector.  Use the weights to create an average of
        // two desired (orthogonal axis) up vectors.  Normalize this average
        // vector to create a combined view up vector to use.
        Vector3f kV = new Vector3f(kViewdirVector);

        kV.set( 1 - Math.abs(kV.X), 1 - Math.abs(kV.Y), 1 - Math.abs(kV.Z) );

        Vector3f kViewupVector = new Vector3f(0.0f, 0.0f, 0.0f);

        kViewupVector.scaleAdd(m_kViewup1.dot(kV), m_kViewup1, kViewupVector);
        kViewupVector.scaleAdd(m_kViewup2.dot(kV), m_kViewup2, kViewupVector);
        kViewupVector.normalize();

        // Project the view-up vector onto the plane which is
        // perpendicular to the view direction vector.  By getting to
        // this point, we know that the view-up vector and the view
        // direction vectors are not aligned.  This projected vector is
        // normalized and becomes the new view-up vector.
        Vector3f kViewdirProjection = Vector3f.scale(kViewdirVector.dot(kViewupVector), kViewdirVector);
        kViewupVector.sub(kViewdirProjection);
        kViewupVector.normalize();
        
        m_kViewPoint.copy(kViewPoint);
        m_kViewDirection.copy(kViewdirVector);
        m_kViewUp.copy(kViewupVector);
    }
}
