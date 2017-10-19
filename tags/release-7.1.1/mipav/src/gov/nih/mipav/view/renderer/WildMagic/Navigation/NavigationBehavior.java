package gov.nih.mipav.view.renderer.WildMagic.Navigation;

import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphCurve;

import java.awt.Toolkit;
import java.awt.event.*;
import javax.swing.*;
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
	
	private MouseWheel mouse;
	
	public NavigationBehavior(VolumeTriPlanarRender _parentScene,
			Camera _m_spkCamera) {

		parentScene = _parentScene;
		camera = _m_spkCamera;

		m_kViewPoint = camera.GetLocation();
		m_kViewDirection = camera.GetDVector();
		m_kViewUp = camera.GetUVector();

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
		currEventTime = event.getWhen();
		if (keyPressdown == false ) {
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
			if (SwingUtilities.isRightMouseButton(e)) {
				currEventTime = e.getWhen();
				if (pressed == false ) {
					pressed = true;
					RightMouse mouse = new RightMouse(e);
					prevEventTime = e.getWhen();
					mouse.start();
				}
			}

			if (SwingUtilities.isLeftMouseButton(e)) {
				pressed = false;
				if ( mouse != null ) { 
					mouse.terminate();
					mouse = null;
				}
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

					// parentScene.updateSceneNodePoint("StartPoint", firstIntersectionPoint);
					parentScene.updateSceneNodePoint("EndPoint", secondIntersectionPoint);

					// System.err.println("firstIntersectionPoint = " + firstIntersectionPoint);
					// System.err.println("secondIntersectionPoint = " + secondIntersectionPoint);
					
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

					// parentScene.updateSceneNodePoint("StartPoint", firstIntersectionPoint);
					parentScene.updateSceneNodePoint("EndPoint", secondIntersectionPoint);

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
		pressed = false;
		m_kViewPoint.copy(camera.GetLocation());
		updateSliceCenter(camera.GetLocation());
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

		if (pressed == false && isDoPicking == false) {
			pressed = true;
			mouse = new MouseWheel(e);
			prevEventTime = e.getWhen();
			mouse.start();
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
		keyPressdown = false;
		m_kViewPoint.copy(camera.GetLocation());
		updateSliceCenter(camera.GetLocation());
		m_kViewRight.copy(camera.GetRVector());
		m_kViewUp.copy(camera.GetUVector());
		m_kViewDirection.copy(camera.GetDVector());
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
			kRotate.fromAxisAngle(kRight, (float) Math.toRadians(cameraViewRotationDegree));
			kRotate.mult(m_kViewDirection, m_kViewDirection);
			kRotate.mult(m_kViewUp, m_kViewUp);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_VIEW);
		} else if (command.equals("lookdown")) {
			// pitch - look down
		    Vector3f kRight = Vector3f.unitCross( m_kViewDirection, m_kViewUp );
            Matrix3f kRotate = new Matrix3f();
            kRotate.fromAxisAngle( kRight, (float)Math.toRadians(-cameraViewRotationDegree) );
            kRotate.mult( m_kViewDirection, m_kViewDirection );
            kRotate.mult( m_kViewUp, m_kViewUp );
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_VIEW);
		} else if (command.equals("lookleft")) {
			// yaw - look left
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(m_kViewUp, (float) Math.toRadians(cameraViewRotationDegree));
			kRotate.mult(m_kViewDirection, m_kViewDirection);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_VIEW);
		} else if (command.equals("lookright")) {
			// case KeyEvent.VK_RIGHT:
			// yaw - look right
			Matrix3f kRotate = new Matrix3f();
			kRotate.fromAxisAngle(m_kViewUp, (float) Math.toRadians(-cameraViewRotationDegree));
			kRotate.mult(m_kViewDirection, m_kViewDirection);
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_VIEW);
		} else if (command.equals("counterclockwise")) {
			// case KeyEvent.VK_F3:
			// roll - counterclockwise
			 Matrix3f kRotate = new Matrix3f();
	            kRotate.fromAxisAngle( m_kViewDirection, (float)Math.toRadians(-cameraViewRotationDegree) );
	            kRotate.mult( m_kViewUp,  m_kViewUp );
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_VIEW);
		} else if (command.equals("clockwise")) {
			// roll - clockwise
			   Matrix3f kRotate = new Matrix3f();
	            kRotate.fromAxisAngle( m_kViewDirection, (float)Math.toRadians(cameraViewRotationDegree) );
	            kRotate.mult( m_kViewUp,  m_kViewUp );
			// Notify listener that we are updated.
			notifyCallback(EVENT_CHANGE_VIEW);
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

				
				evt = new KeyEvent(textField, KeyEvent.KEY_PRESSED, when, 0,  currentEvent.getKeyCode(), currentEvent.getKeyChar());
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
		boolean moveForward = true;

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

}