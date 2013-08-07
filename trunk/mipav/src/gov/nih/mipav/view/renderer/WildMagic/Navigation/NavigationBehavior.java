package gov.nih.mipav.view.renderer.WildMagic.Navigation;

import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphCurve;

import java.awt.Toolkit;
import java.awt.event.*;

import javax.swing.SwingUtilities;

import WildMagic.LibFoundation.Curves.Curve3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Rendering.Camera;
import WildMagic.LibFoundation.Mathematics.*;

/**
 * Behavior which allows for flying down a specified path and looking around.
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
	 * How far ahead down the path should the view be aimed. If zero, then the
	 * view direction is the tangent to the curve. If positive, the the view
	 * direction is to look at that point along the path.
	 */
	public float m_fGazeDist;

	/**
	 * What is the increment in distance along the path that each step takes.
	 * Increment may be negative indicating that the path is being followed in
	 * the opposite direction.
	 */
	public float m_fPathStep;

	/**
	 * When a branch point along the current branch is reached, this flag is set
	 * until the user selects a branch to follow.
	 */
	private boolean m_bChooseBranch = false;

	/** Current annotation in the list. */
	private int m_iAnnotateListItemSelected = -1;

	/** Index of the closest branch at a fork in the path. */
	private int m_iBranchChoiceIndex = -1;

	/**
	 * Instance which implements the Callback interface whose viewChanged method
	 * is to be called whenever anything about this behavior changes.
	 */
	private Callback m_kCallback = null;

	/** Keep reference to instance which describes the path. */
	private FlyPathGraphCurve m_kFlyPathGraph;
	/**
	 * The desired view up vector is a normalized average of these two
	 * orthogonal axes vectors. The problem is that if just one desired view up
	 * vector is chosen, then when the view direction vector is "aligned" with
	 * that vector, some other view up vector would have to be chosen and some
	 * rule would have to be defined for that.
	 */
	private Vector3f m_kViewup1 = new Vector3f(0.0f, 1.0f, 0.0f);
	/** For calculating the view up vector. */
	private Vector3f m_kViewup2 = new Vector3f(0.0f, 0.0f, 1.0f);

	/** Parent frame references. */
	private VolumeTriPlanarRender parentScene;

	/** Current view position along the path. */
	private Vector3f m_kViewPoint = new Vector3f(0f, 0f, 0f);

	/** Current view direction along the path. */
	private Vector3f m_kViewDirection = new Vector3f(0f, 0f, 0f);

	/** Current view up vector. */
	private Vector3f m_kViewUp = new Vector3f(0f, 0f, 0f);

	private boolean m_bMoveForward = true;
	public float m_fNormalizedPathDist;

	private Camera camera;

	/** Picking support: */
	protected int m_iXPick = -1, m_iYPick = -1;

	private boolean pressed;
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
	
	private VolumeShaderEffectMultiPassDynamicCPU shaderEffectCPU;
	
	private boolean isDoPicking = false;
	
	/**
	 * Setup to fly along the specified path and look around.
	 * 
	 * @param kFlyPathGraph
	 *            FlyPathGraphCurve contains the information regarding the graph
	 *            representation of 3D path segments represented by Curve3
	 *            instances
	 * @param kAnnotateList
	 *            FlyPathAnnotateList contains the list of annotation points.
	 * @param kTransformPosition
	 *            TransformGroup contains the Transform3D for the current
	 *            viewing position.
	 * @param kTransformDirection
	 *            TransformGroup contains the Transform3D for the current
	 *            viewing direction.
	 * @param kTransformOrientation
	 *            TransformGroup contains the Transform3D for the current
	 *            viewing orientation.
	 * @param _parentScene
	 *            the parent frame which hold Canvas3D.
	 */
	public NavigationBehavior(VolumeTriPlanarRender _parentScene,
			Camera _m_spkCamera) {

		// Keep references to these.
		parentScene = _parentScene;

		// thresholdTracing = new DetectThreshold(_parentScene);
		// Set initial branch, position, and step.
		// Compute the distance increment along the path that each
		// step will take.
		m_fPathStep = 1.0f;
		m_fGazeDist = 10.0f;

		camera = _m_spkCamera;

		m_kViewPoint = camera.GetLocation();
		m_kViewDirection = camera.GetDVector();
		m_kViewUp = camera.GetUVector();
		
		shaderEffectCPU = parentScene.getShaderEffectCPU();

	}

	
	public void setNaviMode(boolean isNavigationEnabled) {
		if (isNavigationEnabled) {
			// parentScene.GetCanvas().addKeyListener(this);
			parentScene.GetCanvas().addMouseListener(this);
			parentScene.GetCanvas().addMouseMotionListener(this);
			parentScene.GetCanvas().addMouseWheelListener(this);
		} else {
			// parentScene.GetCanvas().removeKeyListener(this);
			pressed = false;
			parentScene.GetCanvas().removeMouseListener(this);
			parentScene.GetCanvas().removeMouseMotionListener(this);
			parentScene.GetCanvas().removeMouseWheelListener(this);
		}
	}

	/**
	 * Get the current distance ahead for looking down the path. If this
	 * distance is zero, then the view direction is the tangent to the path
	 * curve at the current position.
	 * 
	 * @return float Distance ahead for looking down the path.
	 */
	public float getGazeDistance() {
		return m_fGazeDist;
	}

	/**
	 * Get the normalized distance along the current path.
	 * 
	 * @return float Value in the range [0,1].
	 */
	public float getNormalizedPathDistance() {
		return m_fNormalizedPathDist;
	}

	/**
	 * Get the current position distance along the path.
	 * 
	 * @return float path distance.
	 */
	public float getPathDist() {
		return m_fNormalizedPathDist;
	}

	/**
	 * Get the current distance along the path.
	 * 
	 * @return distance along the path.
	 */
	public float getPathDistance() {
		return getNormalizedPathDistance() * getPathLength();
	}

	/**
	 * Get the total length of the current path.
	 * 
	 * @return float Length of the path.
	 */
	public float getPathLength() {
		// return m_kBranchCurve.GetTotalLength();
		return 100;
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
	 * @return   tracking point location
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

		Matrix3f kRotate = new Matrix3f();
		Vector3f kRight;
		if (KeyEvent.KEY_PRESSED == event.getID()) {
			int iKeyCode = event.getKeyCode();
			char iKeyChar = event.getKeyChar();
			switch (iKeyCode) {

			case KeyEvent.VK_ESCAPE:
				setIdentityViewOrientation();
				break;

			case KeyEvent.VK_UP:

				// move forward along the path
				if (!m_bChooseBranch) {
					// doPathStep(1);
					moveStepsZ(1);

				} else {
					beep();
				}

				break;

			case KeyEvent.VK_DOWN:

				// move backward along the path
				if (!m_bChooseBranch) {
					// doPathStep(-1);
					moveStepsZ(-1);
				} else {
					beep();
				}

				break;
			case KeyEvent.VK_8:
				moveStepsY(1);
				break;
			case KeyEvent.VK_2:
				moveStepsY(-1);
				break;
			case KeyEvent.VK_4:
				moveStepsX(1);
				break;
			case KeyEvent.VK_6:
				moveStepsX(-1);
				break;
			case KeyEvent.VK_LEFT:
				// case KeyEvent.VK_F3:
				// roll - counterclockwise
				kRotate.fromAxisAngle(m_kViewDirection,
						(float) Math.toRadians(1));
				kRotate.mult(m_kViewUp, m_kViewUp);
				// Notify listener that we are updated.
				notifyCallback(EVENT_CHANGE_POSITION);
				break;
			case KeyEvent.VK_RIGHT:
				// roll - clockwise
				kRotate.fromAxisAngle(m_kViewDirection,
						(float) Math.toRadians(-1));
				kRotate.mult(m_kViewUp, m_kViewUp);
				// Notify listener that we are updated.
				notifyCallback(EVENT_CHANGE_POSITION);
				break;

			case KeyEvent.VK_F1:
				// pitch - look up
				kRight = Vector3f.unitCross(m_kViewDirection, m_kViewUp);
				kRotate = new Matrix3f();
				kRotate.fromAxisAngle(kRight, (float) Math.toRadians(1));
				kRotate.mult(m_kViewDirection, m_kViewDirection);
				kRotate.mult(m_kViewUp, m_kViewUp);
				// Notify listener that we are updated.
				notifyCallback(EVENT_CHANGE_POSITION);
				break;
			case KeyEvent.VK_F2:
				// pitch - look down
				kRight = Vector3f.unitCross(m_kViewDirection, m_kViewUp);
				kRotate = new Matrix3f();
				kRotate.fromAxisAngle(kRight, (float) Math.toRadians(-1));
				kRotate.mult(m_kViewDirection, m_kViewDirection);
				kRotate.mult(m_kViewUp, m_kViewUp);
				// Notify listener that we are updated.
				notifyCallback(EVENT_CHANGE_POSITION);
				break;
			case KeyEvent.VK_F3:
				// yaw - look left
				kRotate = new Matrix3f();
				kRotate.fromAxisAngle(m_kViewUp, (float) Math.toRadians(1));
				kRotate.mult(m_kViewDirection, m_kViewDirection);
				// Notify listener that we are updated.
				notifyCallback(EVENT_CHANGE_POSITION);
				break;
			case KeyEvent.VK_F4:
				// case KeyEvent.VK_RIGHT:
				// yaw - look right
				kRotate = new Matrix3f();
				kRotate.fromAxisAngle(m_kViewUp, (float) Math.toRadians(-1));
				kRotate.mult(m_kViewDirection, m_kViewDirection);
				// Notify listener that we are updated.
				notifyCallback(EVENT_CHANGE_POSITION);
				break;
			}
		}

	}

	public void mouseClicked(MouseEvent e) {

	}

	
	
	public void mousePressed(MouseEvent e) {

		if (e.isShiftDown()) {
			pressed = false;
		} else {
			if (SwingUtilities.isRightMouseButton(e)) {
				currEventTime = e.getWhen();

				pressed = true;
				RightMouse mouse = new RightMouse(e);
				prevEventTime = e.getWhen();
				mouse.start();
			}

			if (SwingUtilities.isLeftMouseButton(e)) {
				pressed = false;
				if ( e.isControlDown() ) {
					int x = e.getX();
					int y = e.getY();
					Vector3f kPos = new Vector3f(0, 0, 10);
					Vector3f kDir = new Vector3f(0, 0, 1);
					doPick(x, y, kPos, kDir, firstPoint, secondPoint, resultPoint, findPickingPoint);
				}
			} 
		}

	}

	/*
	 * Solve the picking issue in volume opacity histogram or multi-histogram
	 */
	public void doPick(int x, int y, Vector3f kPos, Vector3f kDir,
			Vector3f first, Vector3f second, Vector3f detectPoint, boolean[] findPickingPoint) {

		isDoPicking = true;
		Vector3f start;
		Vector3f end;
				
		Vector3f firstIntersectionPoint = new Vector3f();
		Vector3f secondIntersectionPoint = new Vector3f();
		
		shaderEffectCPU.createProgramText();
		
		if (camera.GetPickRay(x, y, parentScene.GetWidth(), parentScene.GetHeight(), kPos, kDir)) {

			for (int i = 0; i < parentScene.getDisplayList().size(); i++) {
				naviPicker.Execute(
						parentScene.getDisplayList().get(i).GetScene(), kPos,
						kDir, 0.0f, Float.MAX_VALUE);
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

					parentScene.updateSceneNodePoint("StartPoint", firstIntersectionPoint);
					parentScene.updateSceneNodePoint("EndPoint", secondIntersectionPoint);

					System.err.println("firstIntersectionPoint = " + firstIntersectionPoint);
					System.err.println("secondIntersectionPoint = " + secondIntersectionPoint);
					
					// show the threshold detection point
					tracingThreshold(firstIntersectionPoint,
							secondIntersectionPoint, detectPoint, findPickingPoint);
   
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
					parentScene.updateSceneNodePoint("EndPoint", end);

					firstIntersectionPoint.copy(kPos);
					secondIntersectionPoint.copy(end);

					parentScene.updateSceneNodePoint("StartPoint", firstIntersectionPoint);
					parentScene.updateSceneNodePoint("EndPoint", secondIntersectionPoint);

					// show the threshold detection point
					tracingThreshold(firstIntersectionPoint,
							secondIntersectionPoint, detectPoint, findPickingPoint);

					if ( findPickingPoint[0] ) {
					  first.copy(detectPoint);
					  second.copy(secondIntersectionPoint);
					} else {
					  first.copy(secondIntersectionPoint);
					  second.copy(secondIntersectionPoint);
					}
					break;

				case 0:
					// do nothing
					break;
				}

			}

		}
        isDoPicking = false;
	}

	public void tracingThreshold(Vector3f firstIntersectionPoint,
			Vector3f secondIntersectionPoint, Vector3f bestLocation, boolean[] findPickingPoint) {

		Vector3f startImagespace = new Vector3f();
		Vector3f endImageSpace = new Vector3f();
		Vector3f delta = new Vector3f();
		Vector3f currentLocation = new Vector3f();

		float intensity;
		float value;
		float pre_value = 0;
		TransferFunction transfer = parentScene.getParent().getTransferFunction();

		float d;

		boolean found = false;

		startImagespace.copy(firstIntersectionPoint);
		endImageSpace.copy(secondIntersectionPoint);

		// translate the start and end points to image space
		delta = delta.sub(secondIntersectionPoint, firstIntersectionPoint);
		delta.scale(0.002f);

		currentLocation.copy(firstIntersectionPoint);

		found = false;
		
		 float[] afData = new float[16];
		 parentScene.GetRenderer().SetConstantVPMatrix (0, afData);
		 Matrix4f kVP = new Matrix4f( afData, true );
			// kWorld World-view-projection matrix
		 Matrix4f kWVPMatrix = Matrix4f.mult(parentScene.getSceneToWorldMatrix(), kVP);
			
		for (int w = 0; w < 500; w++) {

			Vector3f imageLocation = new Vector3f();
		
			// imageLocation = kWVPMatrix.mult(new Vector4f(currentLocation.X, currentLocation.Y, currentLocation.Z, 1.0f)).getVector3();
			
			imageLocation.copy(currentLocation);
		    imageLocation.sub(parentScene.getTranslate());
			imageLocation.X *= 1.0f / parentScene.getNormalizedXDim();
			imageLocation.Y *= 1.0f / parentScene.getNormalizedYDim();
			imageLocation.Z *= 1.0f / parentScene.getNormalizedZDim();
			imageLocation.X *= (parentScene.getImage().getExtents()[0] - 1);
			imageLocation.Y *= (parentScene.getImage().getExtents()[1] - 1);
			imageLocation.Z *= (parentScene.getImage().getExtents()[2] - 1);

			// System.err.println("w = " + w + " imageL.X = " + imageLocation.X
			//   + " imageL.Y = " + imageLocation.Y + " imageL.Z = " +
			// imageLocation.Z);
			// if out of bounds, just ignore
		
			if ((int)imageLocation.X < 0
					|| (int)imageLocation.X > (parentScene.getImage().getExtents()[0] - 1)
					|| (int)imageLocation.Y < 0
					|| (int)imageLocation.Y > (parentScene.getImage().getExtents()[1] - 1)
					|| (int)imageLocation.Z < 0
					|| (int)imageLocation.Z > (parentScene.getImage().getExtents()[2] - 1)) {
				currentLocation.add(delta);
				continue;
			}
			
			// Vector4f color = thresholdTracing.getColorVolumeShaderMultiPass(new Vector3f((int)imageLocation.X, (int)imageLocation.Y, (int)imageLocation.Z));
			// System.err.println("test color = " + color);
			
			Vector4f color = shaderEffectCPU.p_VolumeShaderMultiPass(currentLocation);
			if ( color != null && (color.X != 0f && color.Y != 0f && color.Z != 0f)) {
			   	System.err.println("test color = " + color);
			   	parentScene.updateSceneNodePoint("Threshold", currentLocation);
			   	if ( color.X >= 0.7f &&  color.Y >= 0.7f && color.Z >= 0.7f ) {
			   		bestLocation.copy(currentLocation);
			   		found = true;
			   		System.err.println("Multi-histo finding");
			   		break;
			   	}
			   	
			}
			
			// search from opacity histogram
			intensity = parentScene.getImage().getFloat(
					(int) imageLocation.X, (int) imageLocation.Y,
					(int) imageLocation.Z);
			value = transfer.getRemappedValue(intensity, 256);

			// System.err.println("value = " + value);
			d = Math.abs(value - pre_value);
			if (d > 20) {
				bestLocation.copy(currentLocation);
				parentScene.updateSceneNodePoint("Threshold", bestLocation);
				found = true;
				System.err.println("Opacity threshold finding");
				break;
			}
			pre_value = value;
			currentLocation.add(delta);

			
		}
		
		System.err.println("find = " + found);
		findPickingPoint[0] = found;
	}

	public void mouseReleased(MouseEvent e) {
		pressed = false;
		m_kViewPoint.copy(camera.GetLocation());
		m_kViewRight.copy(camera.GetRVector());
		m_kViewUp.copy(camera.GetUVector());
		m_kViewDirection.copy(camera.GetDVector());
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

		/*
		if ((currEventTime - prevEventTime) < 200) {
			pressed = false;
			return;
		}
          */
		if (pressed == false && isDoPicking == false) {

			/*
			 * Matrix3f currentRotation = parentScene.getObjectRotation();
			 * 
			 * Vector3f kCDir = camera.GetDVector(); Vector3f kCUp =
			 * camera.GetUVector(); Vector3f kCRight = camera.GetRVector();
			 * Vector3f kCLoc = camera.GetLocation();
			 * 
			 * Matrix3f rotationInverse = Matrix3f.inverse(currentRotation);
			 * 
			 * Vector3f cameraLocationInverse = rotationInverse.mult(kCLoc);
			 * Vector3f cameraDirInverse = rotationInverse.mult(kCDir); Vector3f
			 * cameraUpInverse = rotationInverse.mult(kCUp); Vector3f
			 * cameraRightInverse = rotationInverse.mult(kCRight);
			 */
			// camera.SetFrame(cameraLocationInverse, cameraDirInverse,
			// cameraUpInverse, cameraRightInverse);

			pressed = true;
			MouseWheel mouse = new MouseWheel(e);
			prevEventTime = e.getWhen();
			mouse.start();
		}

	}

	private void moveStepsZ(int stepSize) {
		m_kViewPoint.Z += stepSize * 0.005;
		notifyCallback(EVENT_CHANGE_POSITION);
	}

	private void moveStepsY(int stepSize) {
		m_kViewPoint.Y += stepSize * 0.005;
		notifyCallback(EVENT_CHANGE_POSITION);
	}

	private void moveStepsX(int stepSize) {
		m_kViewPoint.X += stepSize * 0.005;
		notifyCallback(EVENT_CHANGE_POSITION);
	}

	private void makeMove(Vector3f _currentLocation) {
		m_kViewPoint.copy(_currentLocation);
		m_kViewRight.copy(camera.GetRVector());
		m_kViewUp.copy(camera.GetUVector());
		m_kViewDirection.copy(camera.GetDVector());
		notifyCallback(EVENT_CHANGE_POSITION);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.event.KeyListener#keyReleased(java.awt.event.KeyEvent)
	 */
	@Override
	public void keyReleased(KeyEvent event) {
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
			kRotate.fromAxisAngle(kRight, (float) Math.toRadians(1));
			kRotate.mult(m_kViewDirection, m_kViewDirection);
			kRotate.mult(m_kViewUp, m_kViewUp);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_POSITION);
		} else if (command.equals("lookdown")) {
			// pitch - look down
			Vector3f kRight = Vector3f.unitCross(m_kViewDirection, m_kViewUp);
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(kRight, (float) Math.toRadians(-1));
			kRotate.mult(m_kViewDirection, m_kViewDirection);
			kRotate.mult(m_kViewUp, m_kViewUp);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_POSITION);
		} else if (command.equals("lookleft")) {
			// yaw - look left
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(m_kViewUp, (float) Math.toRadians(1));
			kRotate.mult(m_kViewDirection, m_kViewDirection);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_POSITION);
		} else if (command.equals("lookright")) {
			// case KeyEvent.VK_RIGHT:
			// yaw - look right
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(m_kViewUp, (float) Math.toRadians(-1));
			kRotate.mult(m_kViewDirection, m_kViewDirection);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_POSITION);
		} else if (command.equals("counterclockwise")) {
			// case KeyEvent.VK_F3:
			// roll - counterclockwise
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(m_kViewDirection, (float) Math.toRadians(-0.01));
			kRotate.mult(m_kViewUp, m_kViewUp);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_POSITION);
		} else if (command.equals("clockwise")) {
			// roll - clockwise
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(m_kViewDirection, (float) Math.toRadians(0.01));
			kRotate.mult(m_kViewUp, m_kViewUp);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_POSITION);
		} else if (command.equals("escape")) {
			// VK_ESCAPE
			setIdentityViewOrientation();
		}
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
		notifyCallback(EVENT_CHANGE_ALL);
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

	public void tracingMultihisto(Vector3f firstIntersectionPoint, Vector3f secondIntersectionPoint, Vector3f bestLocation) {
		
    	Vector3f startImagespace = new Vector3f();
    	Vector3f endImageSpace = new Vector3f();
    	Vector3f delta = new Vector3f();
    	Vector3f currentLocation = new Vector3f();
		
		float intensity;
		float value;
		float pre_value = 0;
		TransferFunction transfer = parentScene.getParent().getTransferFunction();
		
		float d;
		
		boolean found = false;
		
		startImagespace.copy(firstIntersectionPoint);
		endImageSpace.copy(secondIntersectionPoint);
		
	    // translate the start and end points to image space
		delta = delta.sub(secondIntersectionPoint, firstIntersectionPoint);
		delta.scale(0.002f);
		
		
		currentLocation.copy(firstIntersectionPoint);
	
		found = false;
		for ( int w = 0; w < 500; w++ ) {

			
			Vector3f imageLocation = new Vector3f();
			imageLocation.copy(currentLocation);
			imageLocation.sub( parentScene.getTranslate() );
			imageLocation.X *= 1.0f/parentScene.getNormalizedXDim();
			imageLocation.Y *= 1.0f/parentScene.getNormalizedYDim();
			imageLocation.Z *= 1.0f/parentScene.getNormalizedZDim();
			imageLocation.X *= (parentScene.getImage().getExtents()[0]-1);
			imageLocation.Y *= (parentScene.getImage().getExtents()[1]-1);
			imageLocation.Z *= (parentScene.getImage().getExtents()[2]-1);
		
			// System.err.println("w = " + w + " imageL.X = " + imageLocation.X 
			// 		 + " imageL.Y = " + imageLocation.Y  + " imageL.Z = " + imageLocation.Z);
			// if out of bounds, just ignore
			if ( (int)imageLocation.X < 0 || imageLocation.X > (parentScene.getImage().getExtents()[0]-1) ||
					(int)imageLocation.Y < 0 || imageLocation.Y > (parentScene.getImage().getExtents()[1]-1) ||
					(int)imageLocation.Z < 0 || imageLocation.Z > (parentScene.getImage().getExtents()[2]-1) )
				continue;
			
			// search from opacity histogram
			intensity = parentScene.getImage().getFloat((int)imageLocation.X, (int)imageLocation.Y, (int)imageLocation.Z);
			value = transfer.getRemappedValue(intensity, 256);

			// System.err.println("value = " + value);
		    d = Math.abs(value-pre_value);
			if ( d > 20 ) {
				bestLocation.copy(currentLocation);
				parentScene.updateSceneNodePoint("Threshold", bestLocation);
				found = true;
	
				break;
			}
			pre_value = value;
			currentLocation.add(delta);
			
			System.err.println("find = " + found);		
		}
	}
	    
	
	
	/**
	 * Reset the view orientation transformation to the identity. That is,
	 * remove all yaw/pitch/roll.
	 */
	private void setIdentityViewOrientation() {
		notifyCallback(EVENT_RESET_ORIENTATION);
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

				when += 10;

				try {
					wait(10);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}

				evt = new MouseEvent(parentScene.GetCanvas(), id, when, mod,
						Math.round(x), Math.round(y), 0, false);

			}

		}
	}

	class MouseWheel extends Thread {

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
		int moveForward;

		/**
		 * Creates new thread and sets up mouse event variables appropriately.
		 * 
		 * @param event
		 *            Original mouse event, from button.
		 */
		public MouseWheel(MouseWheelEvent event) {
			when = event.getWhen();
			currentEvent = event;
			id = MouseEvent.MOUSE_DRAGGED;
			source = event.getSource();
			moveForward = event.getWheelRotation();
			x = event.getX();
			y = event.getY();
			evt = new MouseEvent(parentScene.GetCanvas(),
					MouseEvent.MOUSE_PRESSED, when, mod, x, y, 1, false);
		}

		/**
		 * Runs the thread. While the button is pressed, dispatches mouse
		 * dragged events at a rate consistent with the velocity slider. Once
		 * the mouse is released, <code>pressed</code> will be set to false and
		 * the loop will stop.
		 */
		public synchronized void run() {

			int count = 0;

			Vector3f cameraLocation = new Vector3f();
			Vector3f pickingPointLocation = new Vector3f();
		
			cameraLocation.copy(m_kViewPoint);
			m_kViewRight.copy(camera.GetRVector());
			m_kViewUp.copy(camera.GetUVector());
			m_kViewDirection.copy(camera.GetDVector());
			
			if ( findPickingPoint[0] ) {
				pickingPointLocation.copy(resultPoint);
			} else {
				pickingPointLocation.copy(firstPoint);
			}
			
			deltaForward = Vector3f.sub(pickingPointLocation, cameraLocation);
			deltaBackward = Vector3f.sub(cameraLocation, pickingPointLocation);
			trackingForward = Vector3f.sub(pickingPointLocation, cameraLocation);
			trackingBackward = Vector3f.sub(cameraLocation, pickingPointLocation);
				
			deltaForward.scale(0.002f);
			deltaBackward.scale(0.002f);
			trackingForward.scale(0.003f);
		    trackingBackward.scale(0.003f);

			Vector3f currentLocation = new Vector3f();
			
			if (moveForward < 0) {
				currentLocation.copy(cameraLocation);   // = Vector3f.add(cameraLocation, deltaForward);
				trackingPointLocation.copy(cameraLocation);  //  = Vector3f.add(cameraLocation, trackingForward);
			} else {
				currentLocation.copy(cameraLocation);   //  = Vector3f.add(cameraLocation, deltaBackward);
				trackingPointLocation.copy(cameraLocation); //  = Vector3f.add(cameraLocation, trackingBackward);
			}

			float trackingStep = trackingPointLocation.distance(cameraLocation);
			
			while (pressed) {

				parentScene.GetCanvas().dispatchEvent(evt);

				System.err.println(trackingPointLocation.distance(pickingPointLocation));
				if (moveForward < 0 ) {
					if (trackingPointLocation.distance(pickingPointLocation) <= trackingStep ) {
						pressed = false;
						break;
					}
					makeMove(currentLocation);
					currentLocation = currentLocation.add(deltaForward);
					trackingPointLocation = trackingPointLocation.add(deltaForward);
					count++;
				} else {
					if ( count > 500 ) {
						pressed = false;
						break;
					}
					makeMove(currentLocation);
					currentLocation = currentLocation.add(deltaBackward);
					trackingPointLocation = trackingPointLocation.add(deltaBackward);
					count++;
				}

				when += 50;

				try {
					wait(50);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}

				evt = new MouseEvent(parentScene.GetCanvas(), id, when, mod, x,
						y, 0, false);

			}

		}
	}

}
