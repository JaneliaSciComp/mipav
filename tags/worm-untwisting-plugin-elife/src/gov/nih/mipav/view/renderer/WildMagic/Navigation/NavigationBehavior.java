package gov.nih.mipav.view.renderer.WildMagic.Navigation;

import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphCurve;

import java.awt.Toolkit;
import java.awt.event.*;

import javax.swing.*;

import WildMagic.LibFoundation.Curves.Curve3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Rendering.Camera;


/**
 * Behavior which allows for flying down a specified path and looking around.
 * Adding a few conventions for the path planning fly-thru.
 * 1) Shift + Left mouse press to stop at the current location to fly-thru.  At the path two ends, 
 *    also need to use Shift + Left mouse to switch flying direction.   Apply Shift+Left Mouse press
 *    once, the mouse wheel to control the auto fly-thru direction.   Wheel one move forward to fly
 *    into the screen.   Wheel one move backward to fly out of the screen direction. 
 * 2) In path-planning fly-thru mode, doesn't allow to change the camera view.   If so, it introduces
 *    weird bug that distort the bottom three planner cross-hair location.   
 * 3) When switch to mouse control mode, users can change the camera view location.   When switch back 
 * 	  to path planning fly-thru mode, the view is directed to the first starting point of plan planning
 *    annotated path.   This way to avoid the cross-hair location distortion.   
 * 
 * @author Ruida Cheng
 *
 */
public class NavigationBehavior implements KeyListener, MouseListener,
		MouseMotionListener, MouseWheelListener {

	/**
	 * Instances which want to be notified of updates to this behavior should
	 * implement this interface by providing the viewChanged method
	 * implementation.
	 */
	public static interface Callback {

		/**
		 * ViewChanged callback for flythrough renderers.
		 * 
		 * @param kBehavior
		 *            reference to this MjFlyPathBehavior in which the view
		 *            changed.
		 * @param iEvent
		 *            Bitmask identifies the event(s) which caused the view to
		 *            change. Bitmask created from OR of EVENT_* defintions.
		 */
		void viewChanged(NavigationBehavior kBehavior, int iEvent);
	}

	/**
	 * on a viewChanged event change both the view position and direction vector
	 */
	public static final int EVENT_CHANGE_ALL = 0xffffffff;

	/** on a viewChanged event change just the view position */
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
	 * Instance which implements the Callback interface whose viewChanged method
	 * is to be called whenever anything about this behavior changes.
	 */
	private Callback m_kCallback = null;

	/** Parent frame references. */
	private VolumeTriPlanarRender parentScene;

	/** Current view position along the path. */
	private Vector3f m_kViewPoint = new Vector3f(0f, 0f, 0f);

	/** Current view direction along the path. */
	private Vector3f m_kViewDirection = new Vector3f(0f, 0f, 0f);

	/** Current view up vector. */
	private Vector3f m_kViewUp = new Vector3f(0f, 0f, 0f);

	private boolean m_bMoveForward = true;

	private Camera camera;

	/** Picking support: */
	protected int m_iXPick = -1, m_iYPick = -1;

	private volatile boolean pressed;
	private Vector3f deltaForward = new Vector3f();
	private Vector3f deltaBackward = new Vector3f();
	private Vector3f trackingForward = new Vector3f();
	private Vector3f trackingBackward = new Vector3f();

	Vector3f firstPoint = new Vector3f();
	Vector3f secondPoint = new Vector3f();
	Vector3f resultPoint = new Vector3f();
	boolean[] findPickingPoint = new boolean[1];

	Vector3f m_kViewRight = new Vector3f();

	private NavigationPicker naviPicker = new NavigationPicker();

	private Vector3f trackingPointLocation = new Vector3f();

	private boolean isDoPicking = false;

	private boolean keyPressdown = false;

	private int cameraViewRotationDegree = 3;

	private MouseWheelMouseControl mouseFlythru;
	private MouseWheelPathPlanning mousePathPlanning;

	private boolean isMouseFlythru;
	private boolean isPathFlythru;

	private FlyPathGraphCurve m_kFlyPathGraph;
	private BranchState[] m_akBranchState;
	public float m_fGazeDist;
	public float m_fPathStep;
	private BranchState m_kBranchState = null;
	private Vector3f m_kViewup1 = new Vector3f(0.0f, 1.0f, 0.0f);
	private Vector3f m_kViewup2 = new Vector3f(0.0f, 0.0f, 1.0f);
	private BranchState[] m_akBranchChoice = null;
	private boolean m_bChooseBranch = false;
	private int m_iBranchChoiceIndex = -1;
	
	public NavigationBehavior(VolumeTriPlanarRender _parentScene, Camera _m_spkCamera) {

		parentScene = _parentScene;
		camera = _m_spkCamera;

		m_kViewPoint = camera.GetLocation();
		m_kViewDirection = camera.GetDVector();
		m_kViewUp = camera.GetUVector();

	}

	public void setupPath(FlyPathGraphCurve kFlyPathGraph) {
		m_kFlyPathGraph = kFlyPathGraph;
		// Create array to store the state of each branch.
		int iNumBranches = 1; // kFlyPathGraph.getNumBranches();

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

	public void clearPath() {
		int iNumBranches = 1;
		for (int iBranch = 0; iBranch < iNumBranches; iBranch++) {
			m_akBranchState[iBranch] = null;
		}
		m_akBranchState = null;
		m_kBranchState = null;
		m_kFlyPathGraph = null;

	}
	
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

	}

	
	  /**
     * Move along the path step width.
      * @param  _step  step size.
     */
    public void moveAlongPath(int _step) {
        doPathStep(_step);
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
     * Get the current distance along the path.
     * @return  distance along the path.
     */
    public float getPathDistance() {
        return getNormalizedPathDistance() * getPathLength();
    }
	
    /**
     * Get the normalized distance along the current path.
     * @return  float Value in the range [0,1].
     */
    public float getNormalizedPathDistance() {
        return m_kBranchState.m_fNormalizedPathDist;
    }
    
	
	/**
	 * Get the current distance increment for moving along the path.
	 * 
	 * @return distance increment for moving along path. Always positive
	 *         regardless of which direction moving along the path.
	 */
	public float getPathStep() {
		return m_fPathStep;
	}

	private void setBranch(int iBranch) {
		setBranch(m_akBranchState[iBranch]);
	}

	private void setBranch(BranchState kBranchState) {
		m_kBranchState = kBranchState;
		notifyCallback(EVENT_CHANGE_BRANCH);

		setPathDist(m_kBranchState.m_fNormalizedPathDist);
		setIdentityViewOrientation();
	}

	
	
	
	/**
	 * Update the camera position along the path based on the specified distance
	 * from the beginning.
	 * 
	 * @param fNormalizedDist
	 *            normalized distance from the beginning of the path for the
	 *            location of the camera for viewing
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
		// System.err.println("kViewPoint = " + kViewPoint);
		// System.err.println("kLookatVector = " + kLookatVector);

		// Notify listener that we are updated.
		notifyCallback(EVENT_CHANGE_POSITION);
	}

	/**********
	 * ???????????????????????????????????????????????????? ************** How
	 * to do with it !!!!!!!!!!!!!!!!!!
	 * 
	 * @param kViewPoint
	 * @param kViewdirVector
	 */
	private void setView(Vector3f kViewPoint, Vector3f kViewdirVector) {
		// Use the view direction vector to create positive weights where more
		// weight is given to an axis that has less of a component in the
		// direction vector. Use the weights to create an average of
		// two desired (orthogonal axis) up vectors. Normalize this average
		// vector to create a combined view up vector to use.
		Vector3f kV = new Vector3f(kViewdirVector);

		kV.set(1 - Math.abs(kV.X), 1 - Math.abs(kV.Y), 1 - Math.abs(kV.Z));

		Vector3f kViewupVector = new Vector3f(0.0f, 0.0f, 0.0f);

		kViewupVector.scaleAdd(m_kViewup1.dot(kV), m_kViewup1, kViewupVector);
		kViewupVector.scaleAdd(m_kViewup2.dot(kV), m_kViewup2, kViewupVector);
		kViewupVector.normalize();

		// Project the view-up vector onto the plane which is
		// perpendicular to the view direction vector. By getting to
		// this point, we know that the view-up vector and the view
		// direction vectors are not aligned. This projected vector is
		// normalized and becomes the new view-up vector.
		Vector3f kViewdirProjection = Vector3f.scale(
				kViewdirVector.dot(kViewupVector), kViewdirVector);
		kViewupVector.sub(kViewdirProjection);
		kViewupVector.normalize();

		m_kViewPoint.copy(kViewPoint);
		m_kViewDirection.copy(kViewdirVector);
		m_kViewUp.copy(kViewupVector);
	}

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

	public float getPathLength() {
		return m_kBranchState.m_kBranchCurve.GetTotalLength();
	}

	public void setViewPoint(Vector3f in) {
		m_kViewPoint.copy(in);
	}

	public void setDirection(Vector3f in) {
		m_kViewDirection.copy(in);
	}

	public void setUpVector(Vector3f in) {
		m_kViewUp.copy(in);
	}

	public void setRightVector(Vector3f in) {
		m_kViewRight.copy(in);
	}

	public void setNaviMode(boolean isNavigationEnabled) {
		if (isNavigationEnabled) {
			parentScene.GetCanvas().addKeyListener(this);
			parentScene.GetCanvas().addMouseListener(this);
			parentScene.GetCanvas().addMouseMotionListener(this);
			parentScene.GetCanvas().addMouseWheelListener(this);
		} else {
			pressed = false;
			parentScene.GetCanvas().removeKeyListener(this);
			parentScene.GetCanvas().removeMouseListener(this);
			parentScene.GetCanvas().removeMouseMotionListener(this);
			parentScene.GetCanvas().removeMouseWheelListener(this);
		}
	}

	public void setMouseFlythruMode(boolean _isMouseFlythru) {
		isMouseFlythru = _isMouseFlythru;
	}
	
	public void setPathFlythruMode(boolean _isPathFlythru) {
		isPathFlythru = _isPathFlythru;
		if ( m_kFlyPathGraph  != null ) {
			setupPath(m_kFlyPathGraph);
		}
	}

	/**
	 * Returns the view direction vector at the current position on the path.
	 * 
	 * @return view direction vector at the current position on the path.
	 */
	public Vector3f getViewDirection() {
		return m_kViewDirection;
	}

	/**
	 * Returns the current position on the path.
	 * 
	 * @return current position on the path.
	 */
	public Vector3f getViewPoint() {
		return m_kViewPoint;
	}

	/***
	 * return the tracking point location
	 * 
	 * @return tracking point location
	 */
	public Vector3f getTrackingPoint() {
		return trackingPointLocation;
	}

	public Vector3f getViewRight() {
		return m_kViewRight;
	}

	/**
	 * Returns the view up vector at the current position on the path.
	 * 
	 * @return view up vector at the current position on the path.
	 */
	public Vector3f getViewUp() {
		return m_kViewUp;
	}

	/**
	 * Get indication of whether the movement along the current path is in the
	 * forward or reverse direction.
	 * 
	 * @return boolean True if the movement is along the forward direction along
	 *         the path.
	 */
	public boolean isPathMoveForward() {
		return m_bMoveForward;
	}

	
	
	
	/**
	 * Handle the key pressed event from the text field.
	 * 
	 * @param event
	 *            key event to handle
	 */
	@Override
	public void keyPressed(KeyEvent event) {
		currEventTime = event.getWhen();
		if (keyPressdown == false && isMouseFlythru) {
			keyPressdown = true;
			KeyPressedEvent kEvent = new KeyPressedEvent(event);
			prevEventTime = event.getWhen();
			kEvent.start();
		}

	}

	public void mouseClicked(MouseEvent e) {

	}

	public void mousePressed(MouseEvent e) {
		if (e.isShiftDown()) {
			pressed = false;
		} else {
			if (isMouseFlythru) {
				if (SwingUtilities.isRightMouseButton(e)) {
					currEventTime = e.getWhen();
					if (pressed == false) {
						pressed = true;
						RightMouse mouse = new RightMouse(e);
						prevEventTime = e.getWhen();
						mouse.start();
					}
				}

				if (SwingUtilities.isLeftMouseButton(e)) {
					pressed = false;
					if (mouseFlythru != null) {
						mouseFlythru.terminate();
						mouseFlythru = null;
					} else if (mousePathPlanning != null) {
						mousePathPlanning.terminate();
						mousePathPlanning = null;
					}

					if (e.isControlDown() ) {
						int x = e.getX();
						int y = e.getY();
						Vector3f kPos = new Vector3f(0, 0, 10);
						Vector3f kDir = new Vector3f(0, 0, 1);
						doPick(x, y, kPos, kDir, firstPoint, secondPoint,
								resultPoint, findPickingPoint);
					}
				}
			}
		}

	}

	/*
	 * Solve the picking issue in volume opacity histogram or multi-histogram
	 */
	public void doPick(int x, int y, Vector3f kPos, Vector3f kDir,
			Vector3f first, Vector3f second, Vector3f detectPoint,
			boolean[] findPickingPoint) {

		isDoPicking = true;
		Vector3f start;
		Vector3f end;

		Vector3f firstIntersectionPoint = new Vector3f();
		Vector3f secondIntersectionPoint = new Vector3f();

		if (camera.GetPickRay(x, y, parentScene.GetWidth(),
				parentScene.GetHeight(), kPos, kDir)) {

			for (int i = 0; i < parentScene.getDisplayList().size(); i++) {
				naviPicker.Execute(parentScene.getDisplayList().get(i)
						.GetScene(), kPos, kDir, 0.0f, Float.MAX_VALUE);
				PickRecord record;
				float t1, t2;
				switch (naviPicker.Records.size()) {
				case 2:
					// tracing the path between start and end point on the
					// bounding box
					record = naviPicker.Records.get(0);
					t1 = record.T;

					// test the Barycentric way to compute the intersection
					// point
					/*
					 * Triangles pMesh = (Triangles)record.Intersected; int[]
					 * aiTris = new int[3];
					 * pMesh.GetTriangle(record.Triangle,aiTris); int iV0, iV1,
					 * iV2; iV0 = aiTris[0]; iV1 = aiTris[1]; iV2 = aiTris[2];
					 * Vector3f v0 = new Vector3f();
					 * v0.copy(pMesh.VBuffer.GetPosition3(iV0)); Vector3f v1 =
					 * new Vector3f(); v1.copy(pMesh.VBuffer.GetPosition3(iV1));
					 * Vector3f v2 = new Vector3f();
					 * v2.copy(pMesh.VBuffer.GetPosition3(iV2));
					 * 
					 * v0 = pMesh.World.ApplyForward(v0); v1 =
					 * pMesh.World.ApplyForward(v1); v2 =
					 * pMesh.World.ApplyForward(v2);
					 * 
					 * Vector3f firstPoint =
					 * v0.scale(record.B0).add(v1.scale(record
					 * .B1)).add(v2.scale(record.B2));
					 * System.err.println("firstPoint = " + firstPoint);
					 */

					start = new Vector3f();
					// System.err.println("start t = " + t1);
					start = Vector3f.scale(t1, kDir);
					start.add(kPos);
					// System.err.println("start = " + start);

					record = naviPicker.Records.get(1);
					t2 = record.T;
					// System.err.println("end t = " + t2);
					end = new Vector3f();
					end = Vector3f.scale(t2, kDir);
					end.add(kPos);
					// System.err.println("end = " + end);

					if (t1 < t2) {
						firstIntersectionPoint.copy(start);
						secondIntersectionPoint.copy(end);
					} else {
						firstIntersectionPoint.copy(end);
						secondIntersectionPoint.copy(start);
					}

					// parentScene.updateSceneNodePoint("StartPoint",
					// firstIntersectionPoint);
					parentScene.updateSceneNodePoint("EndPoint",
							secondIntersectionPoint);

					// System.err.println("firstIntersectionPoint = " +
					// firstIntersectionPoint);
					// System.err.println("secondIntersectionPoint = " +
					// secondIntersectionPoint);

					detectPoint.copy(secondIntersectionPoint);
					System.err.println("detectPoint = " + detectPoint);

					first.copy(firstIntersectionPoint);
					second.copy(secondIntersectionPoint);

					break;

				case 1:
					// tracing the path between camera location and the end
					// point on the bounding box

					System.err.println("only one pick point");
					record = naviPicker.Records.get(0);
					t1 = record.T;
					end = new Vector3f();
					end = Vector3f.scale(t1, kDir);
					end.add(kPos);
					// parentScene.updateSceneNodePoint("EndPoint", end);

					firstIntersectionPoint.copy(kPos);
					secondIntersectionPoint.copy(end);

					// parentScene.updateSceneNodePoint("StartPoint",
					// firstIntersectionPoint);
					parentScene.updateSceneNodePoint("EndPoint",
							secondIntersectionPoint);

					detectPoint.copy(secondIntersectionPoint);
					second.copy(secondIntersectionPoint);

					break;

				case 0:
					// do nothing
					break;
				}

			}

		}
		isDoPicking = false;
	}

	public void mouseReleased(MouseEvent e) {
		if (isMouseFlythru) {
			pressed = false;
			m_kViewPoint.copy(camera.GetLocation());
			updateSliceCenter(camera.GetLocation());
			m_kViewRight.copy(camera.GetRVector());
			m_kViewUp.copy(camera.GetUVector());
			m_kViewDirection.copy(camera.GetDVector());
		}
	}

	public void mouseDragged(MouseEvent e) {

	}

	public void mouseEntered(MouseEvent e) {

	}

	public void mouseMoved(MouseEvent e) {

	}

	public void mouseExited(MouseEvent e) {

	}

	public void mouseWheelMoved(MouseWheelEvent e) {

		currEventTime = e.getWhen();

		if ( pressed == false && isDoPicking == false ) {
			
			if ( isMouseFlythru ) {
				pressed = true;
				if ( mousePathPlanning != null ) {
					mousePathPlanning.terminate();
					mousePathPlanning = null;
				}
				mouseFlythru = new MouseWheelMouseControl(e);
				prevEventTime = e.getWhen();
				mouseFlythru.start();
			} else if ( isPathFlythru ) {
				pressed = true;
				if ( mouseFlythru != null ) {
					mouseFlythru.terminate();
					mouseFlythru = null;
				}
				setPathDist(m_kBranchState.m_fNormalizedPathDist);
				setIdentityViewOrientation();
				mousePathPlanning = new MouseWheelPathPlanning(e);
				prevEventTime = e.getWhen();
				mousePathPlanning.start();
			}
		} 

	}

	public void setCamerViewRotationDegree(int degree) {
		cameraViewRotationDegree = degree;
	}

	private void makeMove(Vector3f _currentLocation) {
		m_kViewPoint.copy(_currentLocation);
		notifyCallback(EVENT_CHANGE_POSITION);
	}

	private void updateSliceCenter(Vector3f location) {
		parentScene.updateSlicesCenter(location);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyReleased(KeyEvent event) {
		if (isMouseFlythru) {
			keyPressdown = false;
			m_kViewPoint.copy(camera.GetLocation());
			updateSliceCenter(camera.GetLocation());
			m_kViewRight.copy(camera.GetRVector());
			m_kViewUp.copy(camera.GetUVector());
			m_kViewDirection.copy(camera.GetDVector());
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyTyped(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyTyped(KeyEvent event) {
	}

	/**
	 * Call from the JPanelFlythruMove.
	 * 
	 * @param command
	 *            move command.
	 */
	public void move(String command) {

		if (command.equals("lookup")) {
			// pitch - look up
			Vector3f kRight = Vector3f.unitCross(m_kViewDirection, m_kViewUp);
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(kRight,
					(float) Math.toRadians(cameraViewRotationDegree));
			kRotate.mult(m_kViewDirection, m_kViewDirection);
			kRotate.mult(m_kViewUp, m_kViewUp);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_VIEW);
		} else if (command.equals("lookdown")) {
			// pitch - look down
			Vector3f kRight = Vector3f.unitCross(m_kViewDirection, m_kViewUp);
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(kRight,
					(float) Math.toRadians(-cameraViewRotationDegree));
			kRotate.mult(m_kViewDirection, m_kViewDirection);
			kRotate.mult(m_kViewUp, m_kViewUp);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_VIEW);
		} else if (command.equals("lookleft")) {
			// yaw - look left
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(m_kViewUp,
					(float) Math.toRadians(cameraViewRotationDegree));
			kRotate.mult(m_kViewDirection, m_kViewDirection);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_VIEW);
		} else if (command.equals("lookright")) {
			// case KeyEvent.VK_RIGHT:
			// yaw - look right
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(m_kViewUp,
					(float) Math.toRadians(-cameraViewRotationDegree));
			kRotate.mult(m_kViewDirection, m_kViewDirection);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_VIEW);
		} else if (command.equals("counterclockwise")) {
			// case KeyEvent.VK_F3:
			// roll - counterclockwise
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(m_kViewDirection,
					(float) Math.toRadians(-cameraViewRotationDegree));
			kRotate.mult(m_kViewUp, m_kViewUp);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_VIEW);
		} else if (command.equals("clockwise")) {
			// roll - clockwise
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(m_kViewDirection,
					(float) Math.toRadians(cameraViewRotationDegree));
			kRotate.mult(m_kViewUp, m_kViewUp);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_VIEW);
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
		} else if (command.equals("escape")) {
			// VK_ESCAPE
			setIdentityViewOrientation();
		}
	}

	
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
            // Make sure the distance is in the [0,1] range.
            fNewNormalizedPathDistance = clampNormalizedPathDistance(fNewNormalizedPathDistance);
            m_kBranchState.updateDistUnvisited(fNewNormalizedPathDistance);
            setPathDist(fNewNormalizedPathDistance);
	        
	    }

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
	    
	/**
	 * Identify the instance which implements the Callback interface. The
	 * viewChanged method of the Callback interface will be called whenever
	 * anything about this behavior changes.
	 * 
	 * @param kCallback
	 *            reference to instance implementing the Callback interface; may
	 *            pass null reference to disable the callback
	 */
	public void setupCallback(Callback kCallback) {
		m_kCallback = kCallback;
		// notifyCallback(EVENT_CHANGE_ALL);
	}

	/**
	 * Call the implementation of the callback's notification method, if a
	 * callback instance has been defined.
	 * 
	 * @param iEvent
	 *            Bitmask identifies the event(s) which caused the view to
	 *            change. Bitmask created from OR of EVENT_* defintions.
	 */
	private void notifyCallback(int iEvent) {

		if (null != m_kCallback) {
			m_kCallback.viewChanged(this, iEvent);
		}
	}

	/**
	 * Reset the view orientation transformation to the identity. That is,
	 * remove all yaw/pitch/roll.
	 */
	private void setIdentityViewOrientation() {
		notifyCallback(EVENT_RESET_ORIENTATION);
	}

	class KeyPressedEvent extends Thread {

		/** DOCUMENT ME! */
		KeyEvent currentEvent;

		/** int centerX, centerY;. */
		KeyEvent evt;

		/** DOCUMENT ME! */
		Object source;

		/** DOCUMENT ME! */
		long when;

		/** DOCUMENT ME! */
		int x, y, mod, id;

		JTextField textField = new JTextField(8);

		/**
		 * Creates new thread and sets up mouse event variables appropriately.
		 * 
		 * @param event
		 *            Original mouse event, from button.
		 */
		public KeyPressedEvent(KeyEvent event) {
			when = event.getWhen();
			currentEvent = event;
			id = KeyEvent.KEY_PRESSED;
			source = event.getSource();
			evt = event;

		}

		/**
		 * Runs the thread. While the button is pressed, dispatches mouse
		 * dragged events at a rate consistent with the velocity slider. Once
		 * the mouse is released, <code>pressed</code> will be set to false and
		 * the loop will stop.
		 */
		public synchronized void run() {

			while (keyPressdown) {

				parentScene.GetCanvas().dispatchEvent(evt);

				int iKeyCode = evt.getKeyCode();

				switch (iKeyCode) {

				case KeyEvent.VK_ESCAPE:
					setIdentityViewOrientation();
					break;
				case KeyEvent.VK_UP:
					// pitch - look up
					// System.err.println("look up");
					move("lookup");
					break;
				case KeyEvent.VK_DOWN:
					// pitch - look down
					// System.err.println("look down");
					move("lookdown");
					break;
				case KeyEvent.VK_LEFT:
					// yaw - look left
					// System.err.println("look left");
					move("lookleft");
					break;
				case KeyEvent.VK_RIGHT:
					// yaw - look right
					// System.err.println("look right");
					move("lookright");
					break;
				}

				when += 500;

				try {
					wait(500);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}

				evt = new KeyEvent(textField, KeyEvent.KEY_PRESSED, when, 0,
						currentEvent.getKeyCode(), currentEvent.getKeyChar());
			}

		}
	}

	class RightMouse extends Thread {

		/** DOCUMENT ME! */
		MouseEvent currentEvent;

		/** int centerX, centerY;. */
		MouseEvent evt;

		/** DOCUMENT ME! */
		Object source;

		/** DOCUMENT ME! */
		long when;

		/** DOCUMENT ME! */
		int x, y, mod, id;

		/** flag indicate if the rotation is clockwise or counter clockwise. */
		boolean isClockRotation;

		/**
		 * Creates new thread and sets up mouse event variables appropriately.
		 * 
		 * @param event
		 *            Original mouse event, from button.
		 */
		public RightMouse(MouseEvent event) {
			when = event.getWhen();
			currentEvent = event;
			id = MouseEvent.MOUSE_DRAGGED;
			source = event.getSource();
			evt = event;

		}

		/**
		 * Runs the thread. While the button is pressed, dispatches mouse
		 * dragged events at a rate consistent with the velocity slider. Once
		 * the mouse is released, <code>pressed</code> will be set to false and
		 * the loop will stop.
		 */
		public synchronized void run() {

			while (pressed) {

				parentScene.GetCanvas().dispatchEvent(evt);

				if (SwingUtilities.isRightMouseButton(evt) && !evt.isAltDown()) {
					isClockRotation = true;
				} else if (SwingUtilities.isRightMouseButton(evt)
						&& evt.isAltDown()) {
					isClockRotation = false;
				}

				if (isClockRotation) {
					move("clockwise");
				} else {
					move("counterclockwise");
				}

				when += 200;

				try {
					wait(200);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}

				evt = new MouseEvent(parentScene.GetCanvas(), id, when, mod,
						Math.round(x), Math.round(y), 0, false);

			}

		}
	}

	class MouseWheelPathPlanning extends Thread {

		/** DOCUMENT ME! */
		MouseEvent currentEvent;

		/** int centerX, centerY;. */
		MouseEvent evt;

		/** DOCUMENT ME! */
		Object source;

		/** DOCUMENT ME! */
		long when;

		/** DOCUMENT ME! */
		int x, y, mod, id;
		boolean moveForward = true;

		/**
		 * Creates new thread and sets up mouse event variables appropriately.
		 * 
		 * @param event
		 *            Original mouse event, from button.
		 */
		public MouseWheelPathPlanning(MouseWheelEvent event) {
			when = event.getWhen();
			currentEvent = event;
			id = MouseEvent.MOUSE_DRAGGED;
			source = event.getSource();
			moveForward = event.getWheelRotation() < 0 ? true : false;
			x = event.getX();
			y = event.getY();
			evt = new MouseEvent(parentScene.GetCanvas(),
					MouseEvent.MOUSE_PRESSED, when, mod, x, y, 1, false);
		}

		public void terminate() {
			pressed = false;
		}

		/**
		 * Runs the thread. While the button is pressed, dispatches mouse
		 * dragged events at a rate consistent with the velocity slider. Once
		 * the mouse is released, <code>pressed</code> will be set to false and
		 * the loop will stop.
		 */
		public synchronized void run() {

            while (pressed) {

            	parentScene.GetCanvas().dispatchEvent(evt);

            
            	if ( moveForward ) {
            		move("forward");
            	} else {
            		move("backward");
            	}
        	
                when += 100;
                
                try {
                	wait(100);
                } catch ( InterruptedException e ) {
                    e.printStackTrace();
                }

                evt = new MouseEvent(parentScene.GetCanvas(), id, when, mod, Math.round(x), Math.round(y), 0, false);

            }
			
		}
		
	}
	
	
	class MouseWheelMouseControl extends Thread {

		/** DOCUMENT ME! */
		MouseEvent currentEvent;

		/** int centerX, centerY;. */
		MouseEvent evt;

		/** DOCUMENT ME! */
		Object source;

		/** DOCUMENT ME! */
		long when;

		/** DOCUMENT ME! */
		int x, y, mod, id;
		boolean moveForward = true;

		/**
		 * Creates new thread and sets up mouse event variables appropriately.
		 * 
		 * @param event
		 *            Original mouse event, from button.
		 */
		public MouseWheelMouseControl(MouseWheelEvent event) {
			when = event.getWhen();
			currentEvent = event;
			id = MouseEvent.MOUSE_DRAGGED;
			source = event.getSource();
			moveForward = event.getWheelRotation() < 0 ? true : false;
			x = event.getX();
			y = event.getY();
			evt = new MouseEvent(parentScene.GetCanvas(),
					MouseEvent.MOUSE_PRESSED, when, mod, x, y, 1, false);
		}

		public void terminate() {
			pressed = false;
		}
		
		/**
		 * Runs the thread. While the button is pressed, dispatches mouse
		 * dragged events at a rate consistent with the velocity slider. Once
		 * the mouse is released, <code>pressed</code> will be set to false and
		 * the loop will stop.
		 */
		public synchronized void run() {

			Vector3f cameraLocation = new Vector3f();
			Vector3f pickingPointLocation = new Vector3f();
			long updateCenterTime = 0;
		
			cameraLocation.copy(m_kViewPoint);
			m_kViewRight.copy(camera.GetRVector());
			m_kViewUp.copy(camera.GetUVector());
			m_kViewDirection.copy(camera.GetDVector());
			
			pickingPointLocation.copy(secondPoint);
					
			deltaForward = Vector3f.sub(pickingPointLocation, cameraLocation);
			deltaBackward = Vector3f.sub(cameraLocation, pickingPointLocation);
			trackingForward = Vector3f.sub(pickingPointLocation, cameraLocation);
			trackingBackward = Vector3f.sub(cameraLocation, pickingPointLocation);
				
			deltaForward.scale(0.005f);
			deltaBackward.scale(0.005f);
			trackingForward.scale(0.005f);
		    trackingBackward.scale(0.005f);

			Vector3f currentLocation = new Vector3f();
			
			currentLocation.copy(cameraLocation);
			trackingPointLocation.copy(cameraLocation);
			
			if (moveForward) {
				System.err.println("move forward");
			} else {
				System.err.println("move backward");
			}
			
			while (pressed) {

				parentScene.GetCanvas().dispatchEvent(evt);
				
				if (moveForward) {
					makeMove(currentLocation);
					if ( updateCenterTime == 1000 ) {
					 updateSliceCenter(currentLocation);
					 updateCenterTime = 0;
				    }
					currentLocation = currentLocation.add(deltaForward);
					trackingPointLocation = trackingPointLocation.add(deltaForward);
				} else {
					makeMove(currentLocation);
					if ( updateCenterTime == 1000 ) {
					  updateSliceCenter(currentLocation);
					  updateCenterTime = 0;
				    }
				
					currentLocation = currentLocation.add(deltaBackward);
					trackingPointLocation = trackingPointLocation.add(deltaBackward);
				}

				
				when += 100;
				updateCenterTime += 100;
                
				try {
					wait(100);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
                
				evt = new MouseEvent(parentScene.GetCanvas(), id, when, mod, x,
						y, 0, false);

			}
			makeMove(currentLocation);
			updateSliceCenter(currentLocation);

		}
	}
	

	/**
	 * Branch information.
	 */
	public class BranchState extends Object implements Cloneable {

		/**
		 * List of branch points (normalized path distances where one or more
		 * branches starts).
		 */
		public float[] m_afBranchPoint;

		/** Current direction. True if forward, false if reverse. */
		public boolean m_bMoveForward;

		/** Current position along the path. */
		public float m_fNormalizedPathDist;

		/**
		 * Normalized distance (in range [0,1]) along parent branch where the
		 * specified branch starts.
		 */
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
		 * @param iBranchIndex
		 *            int Index which identifies the branch.
		 * @param kFlyPathGraph
		 *            FlyPathGraphCurve Data structure which contains all of the
		 *            information about each branch and its connections.
		 */
		public BranchState(int iBranchIndex, FlyPathGraphCurve kFlyPathGraph) {
			m_iBranchIndex = iBranchIndex;
			m_kBranchCurve = kFlyPathGraph.getCurvePosition(iBranchIndex);

			m_fDistUnvisitedMin = 0.0f;
			m_fDistUnvisitedMax = 1.0f;

			// m_afBranchPoint = kFlyPathGraph.getBranchPoints(iBranchIndex);
			// m_iParentBranchIndex =
			// kFlyPathGraph.getBranchParentIndex(iBranchIndex);
			// m_fParentBranchPoint =
			// kFlyPathGraph.getBranchParentNormalizedDist(iBranchIndex);

			start();
		}

		/**
		 * Create a copy of this instance.
		 * 
		 * @return BranchState New instance which is a copy of this instance.
		 */
		public BranchState createCopy() {
			return (BranchState) clone();
		}

		/**
		 * Create a copy of this instance which has the same information except
		 * that the state of the moving forward is inverted.
		 * 
		 * @return BranchState New instance which is a copy of this instance
		 *         except that the moving forward flag is inverted.
		 */
		public BranchState createReverseCopy() {
			BranchState kCopy = createCopy();

			kCopy.m_bMoveForward = !kCopy.m_bMoveForward;

			return kCopy;
		}

		/**
		 * Return an index which identifies segment the specified normalized
		 * path distance belongs.
		 * 
		 * @param fNormalizedPathDist
		 *            float Normalized path distance in the range [0,1] along
		 *            the branch path.
		 * 
		 * @return int An index of zero is returned if the distance is before
		 *         the first branch point or if there are no branch points. An
		 *         index of one is returned if the distance is greater than or
		 *         equal to the first branch point but less than or equal to
		 *         second branch point.
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
		 * Return the position of the curve of the point further down the curve
		 * the specified distance in the current heading.
		 * 
		 * @param fDist
		 *            float Distance further down the branch curve in the
		 *            current heading. This value can be negative for a point in
		 *            the reverse heading.
		 * 
		 * @return Point3f Coordinates of the 3D point further down along the
		 *         curve.
		 */
		public Vector3f getForwardNormalizedPosition(float fDist) {
			return m_kBranchCurve.GetPosition(getForwardNormalizedTime(fDist));
		}

		/**
		 * Return the normalized path distance of a point further down the
		 * branch curve in the current heading.
		 * 
		 * @param fForwardDist
		 *            float Distance further down the branch curve in the
		 *            current heading. This value can be negative for a point in
		 *            the reverse heading.
		 * 
		 * @return float Normalized path distance in the [0,1] range for the
		 *         requested point.
		 */
		public float getForwardNormalizedTime(float fForwardDist) {

			// Normalize the input distance.
			float fPathDist = m_fNormalizedPathDist
					* m_kBranchCurve.GetTotalLength();

			return m_bMoveForward ? m_kBranchCurve.GetTime(fPathDist
					+ fForwardDist, 100, 1e-02f) : m_kBranchCurve.GetTime(
					fPathDist - fForwardDist, 100, 1e-02f);

		}

		/**
		 * Reset parameters to start at the beginning of the path moving in the
		 * forward direction.
		 */
		public void start() {
			m_fNormalizedPathDist = 0.0f;
			m_bMoveForward = true;
		}

		/**
		 * Update the range of normalized path distances that have not been
		 * visited. Call this method before changing the current normalized path
		 * distance!
		 * 
		 * @param fNewNormalizedPathDistance
		 *            float Normalized path distance about to be set for this
		 *            branch.
		 */
		public void updateDistUnvisited(float fNewNormalizedPathDistance) {

			if (m_fNormalizedPathDist <= m_fDistUnvisitedMin) {
				m_fDistUnvisitedMin = Math.max(m_fDistUnvisitedMin,
						fNewNormalizedPathDistance);
			}

			if (m_fNormalizedPathDist >= m_fDistUnvisitedMax) {
				m_fDistUnvisitedMax = Math.min(m_fDistUnvisitedMax,
						fNewNormalizedPathDistance);
			}
		}

		/**
		 * Clone the current branch state. Used by the mouse recording process.
		 * 
		 * @return Object
		 * 
		 * @throws InternalError
		 *             DOCUMENT ME!
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
}
