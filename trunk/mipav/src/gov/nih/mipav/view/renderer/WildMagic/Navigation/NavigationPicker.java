package gov.nih.mipav.view.renderer.WildMagic.Navigation;

import java.io.Serializable;
import java.util.Vector;

import WildMagic.LibFoundation.Intersection.IntrLine3Triangle3f;
import WildMagic.LibFoundation.Mathematics.Line3f;
import WildMagic.LibFoundation.Mathematics.Triangle3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Spatial;
import WildMagic.LibGraphics.SceneGraph.Triangles;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.Collision.*;

public class NavigationPicker extends Picker {
	/**  */
	private static final long serialVersionUID = -9171618026409224678L;

	public Vector<PickRecord> Records = new Vector<PickRecord>();

	private Vector3f m_kOrigin, m_kDirection;

	// The following three functions return the index of the record satisfying
	// the constraints. They should be called only if Records.size() > 0.
	// The index i satisfies 0 <= i < Records.size().

	private float m_fTMin, m_fTMax;

	// The value returned if the Get* functions are called when Records has
	// no elements.
	private static final PickRecord ms_kInvalid = null;

	public NavigationPicker() {
		super();
	}

	/*
	 * The linear component is parameterized by P + t*D, where P is a point on
	 * the component (P is the origin), D is a unit-length direction, and t is a
	 * scalar in the interval [tmin,tmax] with tmin < tmax. The P and D values
	 * must be in world coordinates. The choices for tmin and tmax are line:
	 * tmin = -Mathf::MAX_REAL, tmax = Mathf::MAX_REAL ray: tmin = 0, tmax =
	 * Mathf::MAX_REAL segment: tmin = 0, tmax > 0;
	 * 
	 * A call to this function will automatically clear the Records array. If
	 * you need any information from this array obtained by a previous call to
	 * Execute, you must save it first.
	 */
	public void Execute(Spatial pkScene, final Vector3f rkOrigin,
			final Vector3f rkDirection, float fTMin, float fTMax) {
		m_kOrigin = rkOrigin;
		m_kDirection = rkDirection;
		m_fTMin = fTMin;
		m_fTMax = fTMax;
		Records.clear();
		pkScene.UpdateGS();
		ExecuteRecursive(pkScene);
	}

	// The picking occurs recursively by traversing the input scene.
	private void ExecuteRecursive(Spatial pkObject) {
		if (pkObject instanceof TriMesh) {
			TriMesh mesh = (TriMesh) pkObject;
			if (mesh.GetName().equals("RayCastVolume")) {
				Triangles pkMesh = (Triangles) (pkObject);
				if (pkMesh.WorldBound.TestIntersection(m_kOrigin, m_kDirection,
						m_fTMin, m_fTMax)) {
					// Convert the linear component to model-space coordinates.
					Line3f kLine = new Line3f(
							pkMesh.World.ApplyInverse(m_kOrigin),
							pkMesh.World.InvertVector(m_kDirection));

					// Compute intersections with the model-space triangles.
					int iTQuantity = pkMesh.GetTriangleQuantity();
					for (int i = 0; i < iTQuantity; i++) {
						int iV0, iV1, iV2;
						int[] aiTris = new int[3];
						if (!pkMesh.GetTriangle(i, aiTris)) {
							continue;
						}

						iV0 = aiTris[0];
						iV1 = aiTris[1];
						iV2 = aiTris[2];
						Triangle3f kTriangle = new Triangle3f(
								pkMesh.VBuffer.GetPosition3(iV0),
								pkMesh.VBuffer.GetPosition3(iV1),
								pkMesh.VBuffer.GetPosition3(iV2));

						IntrLine3Triangle3f kIntr = new IntrLine3Triangle3f(
								kLine, kTriangle);
						if (kIntr.Find() && m_fTMin <= kIntr.GetLineT()
								&& kIntr.GetLineT() <= m_fTMax) {
							PickRecord kRecord = new PickRecord();
							kRecord.Intersected = pkMesh;
							kRecord.T = kIntr.GetLineT();
							kRecord.Triangle = i;
							kRecord.iV0 = iV0;
							kRecord.iV1 = iV1;
							kRecord.iV2 = iV2;
							kRecord.B0 = kIntr.GetTriB0();
							kRecord.B1 = kIntr.GetTriB1();
							kRecord.B2 = kIntr.GetTriB2();
							Records.add(kRecord);
						}
					}
				}
			}
			return;
		}

		if (pkObject instanceof Node) {
			Node pkNode = (Node) (pkObject);
			if (pkNode.WorldBound.TestIntersection(m_kOrigin, m_kDirection,
					m_fTMin, m_fTMax)) {
				for (int i = 0; i < pkNode.GetQuantity(); i++) {
					Spatial pkChild = pkNode.GetChild(i);
					if (pkChild != null) {
						ExecuteRecursive(pkChild);
					}
				}
			}
		}
	}
}
