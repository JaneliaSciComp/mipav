package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.renderer.J3D.ViewJFrameVolumeView;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelGeodesic_WM;

import java.util.LinkedList;

import javax.swing.JProgressBar;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;


/**
 * @author  Alexandra Bokinsky, Ph.D. Under contract from Magic Software.
 */
public class Geodesic_WM {

    /** Reference to the JPanelGeodesic:. */
    JPanelGeodesic_WM m_kPanel = null;

    /** List of triangles to remove. */
    private boolean[] m_abRemoveTris;

    /** The indices of the triangle containing the end of the new line. */
    private int[] m_aiEndIndex = null;

    /** The indices of the triangle containing the first point on the new line.  */
    private int[] m_aiFirstIndex = null;

    /** vertices array. */
    private int[] m_aiIndex = null;

    /** For shifting the triangle indices after removing triangles from the mesh. */
    private int[] m_aiIndexShift;

    /** The indices of the triangle containing the previous start point.  */
    private int[] m_aiPreviousStartIndex = null;

    /** The indices of the triangle containing the start of the new line. */
    private int[] m_aiStartIndex = null;

    /**
     * Data members used in Dijkstra's search. The Edgelist is a list for each vertex of all the vertices that it is
     * connected to by one edge. The vertices are stored in the Edgelist as the vertex index.
     */
    private LinkedList<Integer>[] m_akEdgeList = null;

    /** Turned on when picking with the mouse is enabled:. */
    private boolean m_bEnabled = false;

    /** Flag to indicates end point changes. */
    private boolean m_bEndpointChanged = false;

    /** Closing the Geodesic path:. */
    private boolean m_bFinished = true;

    @SuppressWarnings("unused")
    private boolean m_bFirstWire = false;

    /** Flag for clearing the Geodesic curves, if it is false, no curves have been added to the GeodesicGroup. */
    private boolean m_bGroupAdded = false;

    /** Live wire last point. */
    private boolean m_bLastWire = false;

    /** Live wire or point and click mode. */
    private boolean m_bLivewire = false;

    /** Close path or not. */
    private boolean m_bOpen = true;

    /** For Dijkstra search. */
    private boolean[] m_bRelaxed = null;

    /** Path length statistics for each type of path:. */
    private float m_fDijkstraPathLength = 0;

    /**
     * Radius of the sphere displayed to mark the points on the Geodesic. This can be set directly by the class using
     * the Geodesic object.
     */
    private float m_fRadius = .10f;

    /** Weights, relaxed flags, and previous vertex index for Dijkstra's search:. */
    private float[] m_fRemainingWeight = null;

    /** Length of the smoothed geodesic path */
    private float m_fSmoothedPathLength = 0;

    /** For Dijkstra search. */
    private float[] m_fWeight = null;

    /** The index of the vertex in the triangle mesh where the Geodesic curve is to end. */
    private int m_iEnd = -1;

    /** index count. */
    private int m_iIndexCount = 0;

    /** The index of the last node on a closed path. */
    private int m_iLineClosed = 0;

    /** Number of vertices in the geodesic curve. */
    private int m_iNumGeodesicVertices = 0;

    /** Number of meshes. */
    private int m_iNumNewMeshes = 0;

    /** Keeps track of the of picking, so that when a pair of points has been picked the Geodesic is calculated:. */
    private int m_iNumPicked = 0;

    /** Number of triangles in the mesh, after the new triangles are added:. */
    private int m_iNumTriangles = 0;

    /** number of vertices in the path. */
    private int m_iNumWorking = 0;

    /** For Dijkstra search. */
    private int[] m_iPrevious = null;

    /** The start index value for the pair of points. */
    private int m_iStart = -1;

    /**
     * Local copies of the Vertex and Index arrays: a local copy is kept so that when the start or end points fall
     * inside a triangle, a new vertex is added to the vertex array, and three new triangles are added to the triangle
     * index array, new normals are added to the Normal array:.
     */
    private int m_iVertexCount = 0;

    /**
     * Data member for Dijkstra's search. The m_kBorder list stores all the vertices that have been visited by
     * Dijkstra's search, but that have not yet been relaxed. It is used to speed up the search for the non-relaxed
     * vertex with the smallest path distance
     */
    private LinkedList<Integer> m_kBorder = null;

    /** The number of line segments on the Dijkstra curve. */
    private int m_iDijkstraGeodesicGroup = 0;

    /** End point on the curve. */
    private Vector3f m_kEndPoint = null;

    /** Surface the End point is on. */
    private TriMesh m_kEndSurface;

    /** The number of line segments on the Euclidian curve. */
    private int m_iEuclidianGeodesicGroup = 0;

    /** The finished TriMesh */
    private TriMesh m_kFinished;

    /** First point, for closing the Geodesic curve:. */
    private Vector3f m_kFirstPoint = null;

    /** For finished paths, either open or closed:. */
    private LinkedList<LinkedList<Vector4f>> m_kGeodesic_Finished = new LinkedList<LinkedList<Vector4f>>();

    /**
     * LinkedLists to contain the working paths in progress, and all finished paths, open and closed for the smoothed
     * geodesics and dijkstra's geodesics. These are used in the cutting operations: For paths that are not finished,
     * points may still be added to these paths:
     */
    private LinkedList<LinkedList<Vector4f>> m_kGeodesic_Working = new LinkedList<LinkedList<Vector4f>>();

    /**
     * LinkedLists to contain the working paths in progress, and all finished paths, open and closed for the smoothed
     * geodesics and dijkstra's geodesics. These are used in the cutting operations: For paths that are not finished,
     * points may still be added to these paths:
     */
    private LinkedList<LinkedList<Integer>> m_kGeodesic_Working_Left = new LinkedList<LinkedList<Integer>>();

    /**
     * LinkedLists to contain the working paths in progress, and all finished paths, open and closed for the smoothed
     * geodesics and dijkstra's geodesics. These are used in the cutting operations: For paths that are not finished,
     * points may still be added to these paths:
     */
    private LinkedList<LinkedList<Integer>> m_kGeodesic_Working_Right = new LinkedList<LinkedList<Integer>>();

    /** The final list of points in the Geodesic curve. All points are constrained to lie on the triangle mesh, */
    private Vector3f[] m_kGeodesicVertices = null;

    /** Previous cut mesh */
    private TriMesh m_kLastCut;

    /** Previous mesh with a finished curve */
    private TriMesh m_kLastFinished;

    /** Current modified mesh */
    private TriMesh m_kModified;

    /** New normal link list. */
    private LinkedList<Vector3f> m_kNewNormals;

    /** New texCoords link list. */
    private LinkedList<Vector3f> m_kNewTexCoords;

    /** New Colors link list. */
    private LinkedList<ColorRGBA> m_kNewColors;

    /** new triangle link list. */
    private LinkedList<Vector3f> m_kNewTriangles;

    /** New vertices link list. */
    private LinkedList<Vector3f> m_kNewVerts;

    /** Data members for the Geodesic Class: Triangle mesh:. */
    private TriMesh m_kOriginal;

    /** Volume renderer progress bar:. */
    private JProgressBar m_kPBar = null;

    /** PickCanvas, created by the class that creates the triangle mesh and the geodesic group:. */
    //private PickCanvas m_kPickCanvas = null;

    /** Color of the first and successive points on the Geodesic curve:. */
    private ColorRGB[] m_kPickColors;

    /** Previous start point on the new line segment. */
    private Vector3f m_kPreviousStartPoint = null;

    /** Removed triangle link list. */
    private LinkedList<Vector3f> m_kRemoveTriangles;

    /** Root group for different path. */
    private int m_iSmoothedGeodesicGroup = 0;

    /** link list to hold the path. */
    private LinkedList<LinkedList<Vector3f>> m_kStartEndList = new LinkedList<LinkedList<Vector3f>>();

    /**
     * Start and End points -- pair of points for which a Geodesic is calculated, must be in TriangleMesh coordinates:.
     */
    private Vector3f m_kStartPoint = null;

    /** TriMesh containing the start point on the new line segment */
    private TriMesh m_kStartSurface;

    /** Current TriMesh surface */
    private TriMesh m_kSurface;

    /** Backup surface. */
    private TriMesh m_kSurfaceBackup;

    /** Number of triangles removed during a cut mesh operation. */
    private int m_iNumRemoved = 0;
    /** Number of triangles not removed during a cut mesh operation. */
    private int m_iNumNotRemoved = 0;

    /**
     * Instantiation without initializing the progress bar, pickCanvas, GeodesicGroup, triangle mesh or sphere radius,
     * each of those can be set through individual member access functions:.
     */
    public Geodesic_WM() {
        initColors();
    }

    /**
     * Instantiation of the Geodesic object, with the objects necessary for the Geodesic to serve as a MouseListener that
     * performs picking and with the Group kGeodesicGroup so that the Geodesic curve can be drawn directly on the
     * TriMesh.
     *
     * @param  kPickCanvas     PickCanvas
     * @param  kGeodesicGroup  Group
     * @param  kMesh           TriMesh surface
     * @param  fRadius         float marker sphere radius
     */
    public Geodesic_WM(TriMesh kMesh, float fRadius) {
        m_kPBar = ViewJFrameVolumeView.getRendererProgressBar();

        setSurface(kMesh);
        setRadius(fRadius);

        initColors();
        m_kPickColors = new ColorRGB[2];
        m_kPickColors[0] = new ColorRGB(1, 0, 0);
        m_kPickColors[1] = new ColorRGB(1, 1, 0);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clear all geodesics curves drawn on the surface.
     *
     * @param  bAll  bAll, when true clears all the geodesic curves, when false, clears the last point drawn:
     */
    public void clear(boolean bAll) {
        int iNode = 0;
        int iNumRemoved = 0;

        if (m_bGroupAdded) {

            /* Clear the last point added: */
            if (bAll == false) {

                if (!m_kStartPoint.isEqual(m_kPreviousStartPoint)) {

                    if (!m_bLastWire) {
                        iNode = m_iSmoothedGeodesicGroup - 1;

                        if (iNode >= 0) {
                            if ( m_kSurface == null )
                            {
                                m_kPanel.removeGeodesic(m_kStartSurface, iNode, -1);
                            }
                            else
                            {
                                m_kPanel.removeGeodesic(m_kSurface, iNode, -1);
                            }
                            m_iSmoothedGeodesicGroup--;
                            m_iDijkstraGeodesicGroup--;
                            m_iEuclidianGeodesicGroup--;
                            iNumRemoved++;
                        }

                        /* If the last iNode is the line connecting the last point
                         * to the first point, closing the curve, so do not delete
                         * the last point, just delete the connecting geodesic: */
                        if ((iNode > 0) && (iNode != (m_iLineClosed + 1))) {
                            iNode--;
                            m_kPanel.removeGeodesic(m_kSurface, iNode, -1);
                            m_iSmoothedGeodesicGroup--;
                            m_iDijkstraGeodesicGroup--;
                            m_iEuclidianGeodesicGroup--;
                            iNumRemoved++;
                        }

                        if ((iNode != 0) && (m_kPreviousStartPoint != null)) {
                            setStartPoint(m_kPreviousStartPoint, false);
                            setStartIndices(m_aiPreviousStartIndex, false);
                        }

                        /* Remove the last segment from the working lists: */
                        m_iNumWorking--;

                        if (m_iNumWorking >= 0) {
                            m_kGeodesic_Working.remove(m_iNumWorking);
                            m_kGeodesic_Working_Left.remove(m_iNumWorking);
                            m_kGeodesic_Working_Right.remove(m_iNumWorking);
                        } else {
                            m_iNumWorking = 0;
                        }
                    } else if (m_bLastWire) {
                        iNode = m_iDijkstraGeodesicGroup - 1;
                        //m_kPanel.removeGeodesic(iNode, 1);
                        /* Remove the last point and the last curve: */
                        m_iDijkstraGeodesicGroup--;
                        m_iEuclidianGeodesicGroup--;
                        iNumRemoved++;
                        iNode = m_iDijkstraGeodesicGroup - 1;
                        //m_kPanel.removeGeodesic(iNode, 1);
                        m_iDijkstraGeodesicGroup--;
                        m_iEuclidianGeodesicGroup--;
                        iNumRemoved++;

                        clearLastStartEnd();
                    }
                }
            }
            /* Clear all points and curves, and disable drawing: */
            else {
                m_bEnabled = false;
                m_kPanel.removeAllGeodesic(m_kSurface);
                m_iSmoothedGeodesicGroup = 0;
                m_iDijkstraGeodesicGroup = 0;
                m_iEuclidianGeodesicGroup = 0;

                m_bLastWire = false;
                clearAllStartEnd();
                iNode = 0;

                /* Clear all path lists: */
                m_kGeodesic_Working.clear();
                m_kGeodesic_Working_Left.clear();
                m_kGeodesic_Working_Right.clear();
                m_kGeodesic_Finished.clear();

                m_iNumWorking = 0;
            }

            /* IF all points are removed: */
            if (iNode <= 0) {
                m_bGroupAdded = false;

                m_iNumGeodesicVertices = 0;
                m_kGeodesicVertices = null;

                m_iNumPicked = 0;
                m_kFirstPoint = null;
                m_aiFirstIndex = null;
            }
        }

        /* IF just the first point on a new line was removed: */
        if ((iNode == (m_iLineClosed + 1)) && (iNumRemoved == 1)) {

            /* Start new line: */
            m_iNumPicked = 0;
        } 
        
        if (bAll) {

            /* replace the modified mesh with the original: */
            m_kPanel.replaceMesh(m_kModified, m_kOriginal);
            m_kStartSurface = null;
            m_kSurface = null;
            m_kModified = null;
        } else {

            /* replace the modified mesh with the backup: */
            m_kPanel.replaceMesh(m_kModified, m_kSurfaceBackup);
            m_kStartSurface = m_kSurfaceBackup;
            m_kModified = m_kSurfaceBackup;
            m_kSurface = m_kSurfaceBackup;
        }

    }

    /**
     * Clear the current geodesics curve drawn on the surface.
     *
     * @param  bAll  bAll, when true clears all the geodesic curves, when false, clears the last point drawn:
     */
    public void clearCut(boolean bAll) {
        clear(bAll);

        if (bAll == false) {

            /* replace the modified mesh with the backup: */
            m_kPanel.replaceMesh(m_kSurfaceBackup, m_kLastCut);
            m_kStartSurface = m_kLastCut;
            m_kModified = m_kLastCut;
            m_kSurface = m_kLastCut;

            m_kFinished = m_kModified;
            m_kLastFinished = m_kModified;
        }

        resetDrawGeodesic();
    }

    /**
     * Compute the Geodesic curve. The triangle mesh, start, and end points, start and end indices must be defined
     * before this function is called.
     *
     * @param   fPercentage  float, the optimization parameter for Dijkstra's shortest-path search. Values between
     *                       0-100, (increasing optimization).
     * @param   bSmoothed    flag to smoothes and stores the shortest path.
     *
     * @return  true on successful path creation.
     */
    public boolean computeGeodesic(  float fPercentage, boolean bSmoothed) {

        /* Initialize the vertex and index arrays, edge lists and weights for
         * the search: */
        initializeGeodesic(fPercentage);
        
        /* set the start point for the search: */
        int iSmallest = m_iStart;
        m_fWeight[m_iStart] = 0;

        int iCount = 1;

        /* Loop until the end point is reached: */
        while ((iSmallest != -1) && (iSmallest != m_iEnd)) {

            /* Set the progress bar indicator: */
            if (m_kPBar != null) {
                m_kPBar.setValue((int) (100 * ((float) iCount++ / (float) m_iVertexCount)));
                m_kPBar.update(m_kPBar.getGraphics());
            }

            /* Relax the edges around the current vertex: */
            relaxEdges(m_kModified, iSmallest);

            /* Find the vertex with the smallest weight and add it to the
             * path: */
            iSmallest = findSmallest();
        }

        /* Set the progress bar indicator: */
        if (m_kPBar != null) {
            m_kPBar.setValue(100);
            m_kPBar.update(m_kPBar.getGraphics());
        }

        /* Smoothes and stores the shortest path: */
        if (bSmoothed) {
            return createPath( m_kModified, m_iStart, m_iEnd);
        }

        return true;
    }

    /**
     * createPath starts with the results of Dijkstra's minimum path algorithm. It smooths the path, and stores the
     * resulting points in the m_kGeodesicPath data member.
     *
     * @param   iStart  int given geodesic line starting point
     * @param   iEnd    int given geodesic line ending point
     *
     * @return  boolean success or not
     */
    public boolean createPath(TriMesh kMesh, int iStart, int iEnd) {

        /* If the start == end, return 0 - no path: */
        if (iStart == iEnd) {
            return true;
        }

        /* The new path will have an unknown number of points, use a
         * LinkedList: */
        LinkedList<Integer> kMiddle = new LinkedList<Integer>();

        /* Store the End index: */
        kMiddle.add(new Integer(iEnd));

        /* kLeft and kRight store the endpoints of the edges that the path
         * intersects, the size of Left and Right should match the size of
         * kNewVert list, also LinkedLists: */
        LinkedList<Integer> kLeft = new LinkedList<Integer>();
        LinkedList<Integer> kRight = new LinkedList<Integer>();

        /* Add "unknown" for left and right for the iEnd: */
        kLeft.add(0, new Integer(-1));
        kRight.add(0, new Integer(-1));


        /* The next step is to loop over the poins in Dijkstra's path. For all
         * points along the path, except the first and last points, create a list of edges that extend to the left and
         * right of that point. If there are unequal numbers of left and right edges, then repeat the last edge for the
         * side with fewer edges:
         */
        LinkedList<Integer> kEndPoints = new LinkedList<Integer>();
        int iNumEndPoints = 0;

        int iNode = iEnd;
        int iPrevious = m_iPrevious[iNode];

        if (iPrevious == -1) {
            System.err.println("error computing geodesic: Start = " + iStart + " End = " + iEnd);

            return false;
        }

        int iPreviousPrev;
        int iLeftPrev = iNode;
        int iRightPrev = iNode;

        while (iPrevious != iStart) {
            boolean bDone = false;
            boolean bLeftDone = false;
            boolean bRightDone = false;
            iPreviousPrev = m_iPrevious[iPrevious];

            iNumEndPoints = findTriPoints(iNode, iPrevious, kEndPoints);

            if (iNumEndPoints == 0) {
                kLeft.add(new Integer(-1));
                kRight.add(new Integer(-1));
                bDone = true;
            } else if (iNumEndPoints == 1) {

                if ((kLeft.size() == 0) && (kRight.size() == 0)) {
                    kLeft.add(new Integer(-1));
                    iLeftPrev = -1;
                    bLeftDone = true;

                    kRight.add(new Integer(kEndPoints.get(0).intValue()));
                    iRightPrev = kEndPoints.get(0).intValue();
                } else {

                    /* Add to left list: */
                    if (kLeft.getLast().equals(kEndPoints.get(0))) {
                        kRight.add(new Integer(-1));
                        iRightPrev = -1;
                        bRightDone = true;

                        kLeft.add(new Integer(kEndPoints.get(0).intValue()));
                        iLeftPrev = kEndPoints.get(0).intValue();
                    }
                    /* Add to right list: */
                    else if (kRight.getLast().equals(kEndPoints.get(0))) {
                        kLeft.add(new Integer(-1));
                        iLeftPrev = -1;
                        bLeftDone = true;

                        kRight.add(new Integer(kEndPoints.get(0).intValue()));
                        iRightPrev = kEndPoints.get(0).intValue();
                    } else {
                        kLeft.add(new Integer(-1));
                        iLeftPrev = -1;
                        bLeftDone = true;

                        kRight.add(new Integer(kEndPoints.get(0).intValue()));
                        iRightPrev = kEndPoints.get(0).intValue();
                    }
                }
            } else {

                if ((kLeft.size() == 0) && (kRight.size() == 0)) {
                    kLeft.add(new Integer(kEndPoints.get(0).intValue()));
                    iLeftPrev = iNode;

                    kRight.add(new Integer(kEndPoints.get(1).intValue()));
                    iRightPrev = iNode;
                } else {

                    if (kLeft.getLast().equals(kEndPoints.get(0)) || kRight.getLast().equals(kEndPoints.get(1))) {
                        kLeft.add(new Integer(kEndPoints.get(0).intValue()));
                        iLeftPrev = iNode;

                        kRight.add(new Integer(kEndPoints.get(1).intValue()));
                        iRightPrev = iNode;
                    } else if (kRight.getLast().equals(kEndPoints.get(0)) ||
                                   kLeft.getLast().equals(kEndPoints.get(1))) {
                        kRight.add(new Integer(kEndPoints.get(0).intValue()));
                        iRightPrev = iNode;

                        kLeft.add(new Integer(kEndPoints.get(1).intValue()));
                        iLeftPrev = iNode;
                    } else {
                        kLeft.add(new Integer(kEndPoints.get(0).intValue()));
                        iLeftPrev = iNode;

                        kRight.add(new Integer(kEndPoints.get(1).intValue()));
                        iRightPrev = iNode;
                    }
                }
            }

            kMiddle.add(new Integer(iPrevious));

            if (bLeftDone && bRightDone) {
                bDone = true;
            }

            while (!bDone) {
                iNode = (kLeft.getLast()).intValue();

                if (iNode != -1) {
                    iNumEndPoints = findTriPoints(iNode, iPrevious, kEndPoints);

                    if (iNumEndPoints == 1) {
                        kLeft.removeLast();
                        kLeft.add(new Integer(-1));
                        kLeft.add(new Integer(-1));
                        iLeftPrev = -1;
                        bLeftDone = true;
                    } else if (kEndPoints.get(0).intValue() != iLeftPrev) {

                        if (kEndPoints.get(0).intValue() != iPreviousPrev) {
                            kLeft.add(new Integer(kEndPoints.get(0).intValue()));
                            iLeftPrev = iNode;
                        } else {
                            kLeft.add(new Integer((kLeft.getLast()).intValue()));
                            bLeftDone = true;
                        }
                    } else if (kEndPoints.get(1).intValue() != iPreviousPrev) {
                        kLeft.add(new Integer(kEndPoints.get(1).intValue()));
                        iLeftPrev = iNode;
                    } else {
                        kLeft.add(new Integer((kLeft.getLast()).intValue()));
                        bLeftDone = true;
                    }
                } else {
                    kLeft.add(new Integer(-1));
                    iLeftPrev = -1;
                    bLeftDone = true;
                }

                iNode = (kRight.getLast()).intValue();

                if (iNode != -1) {
                    iNumEndPoints = findTriPoints(iNode, iPrevious, kEndPoints);

                    if (iNumEndPoints == 1) {
                        kRight.removeLast();
                        kRight.add(new Integer(-1));
                        kRight.add(new Integer(-1));
                        iRightPrev = -1;
                        bRightDone = true;
                    } else if (kEndPoints.get(0).intValue() != iRightPrev) {

                        if (kEndPoints.get(0).intValue() != iPreviousPrev) {
                            kRight.add(new Integer(kEndPoints.get(0).intValue()));
                            iRightPrev = iNode;
                        } else {
                            kRight.add(new Integer((kRight.getLast()).intValue()));
                            bRightDone = true;
                        }
                    } else if (kEndPoints.get(1).intValue() != iPreviousPrev) {
                        kRight.add(new Integer(kEndPoints.get(1).intValue()));
                        iRightPrev = iNode;
                    } else {
                        kRight.add(new Integer((kRight.getLast()).intValue()));
                        bRightDone = true;
                    }
                } else {
                    kRight.add(new Integer(-1));
                    iRightPrev = -1;
                    bRightDone = true;
                }

                if (bLeftDone && bRightDone) {
                    bDone = true;
                }

                kMiddle.add(new Integer(iPrevious));
            }

            /* Move to the next point: */
            iNode = iPrevious;
            iPrevious = m_iPrevious[iNode];
        }

        /* Add iStart, the start index to the polyline: */
        kMiddle.add(new Integer(iStart));

        /* Add "unknown" to the Left and Right edgelists for iStart point: */
        kLeft.add(new Integer(-1));
        kRight.add(new Integer(-1));

        LinkedList<Vector4f> kNewVert = new LinkedList<Vector4f>();
        LinkedList<Integer> kNewLeft = new LinkedList<Integer>();
        LinkedList<Integer> kNewRight = new LinkedList<Integer>();

        /* Store the End index: */
        Vector4f kNewPoint4 =
            new Vector4f( kMesh.VBuffer.GetPosition3fX(iEnd),
                    kMesh.VBuffer.GetPosition3fY(iEnd),
                    kMesh.VBuffer.GetPosition3fZ(iEnd),
                          iEnd );
        kNewVert.add(new Vector4f(kNewPoint4));

        /* Add "unknown" for left and right for the iEnd: */
        kNewLeft.add(new Integer(-1));
        kNewRight.add(new Integer(-1));


        LinkedList<Vector4f> kNewVertTemp = new LinkedList<Vector4f>();
        LinkedList<Integer> kLeftTemp = new LinkedList<Integer>();
        LinkedList<Integer> kRightTemp = new LinkedList<Integer>();
        int iPrevSide = 0;
        int iPath = 1;

        while (iPath < (kMiddle.size() - 1)) {
            int iSide = smoothPath( kMesh, iPath, kLeft, kMiddle, kRight, kLeftTemp, kRightTemp, kNewVertTemp);

            if ((iPath == 1) || (iPrevSide == -1)) {
                iPrevSide = iSide;
            }

            if (iSide != -1) {

                if (iSide != iPrevSide) {
                    kNewLeft.add(new Integer((kMiddle.get(iPath - 1)).intValue()));
                    kNewRight.add(new Integer((kMiddle.get(iPath)).intValue()));

                    int iCoord = (kMiddle.get(iPath)).intValue();
                    kNewVert.add( new Vector4f( kMesh.VBuffer.GetPosition3fX(iCoord),
                            kMesh.VBuffer.GetPosition3fY(iCoord),
                            kMesh.VBuffer.GetPosition3fZ(iCoord),
                                                iCoord));
                }
            }

            for (int j = 0; j < kNewVertTemp.size(); j++) {
                kNewVert.add(new Vector4f(kNewVertTemp.get(j)));
                kNewLeft.add(new Integer(kLeftTemp.get(j).intValue()));
                kNewRight.add(new Integer(kRightTemp.get(j).intValue()));
            }

            kLeftTemp.clear();
            kRightTemp.clear();
            kNewVertTemp.clear();

            while ((kMiddle.get(iPath)).intValue() == (kMiddle.get(iPath + 1)).intValue()) {
                iPath++;
            }

            iPath++;
            iPrevSide = iSide;
        }

        kNewPoint4.set( kMesh.VBuffer.GetPosition3fX(iStart),
                kMesh.VBuffer.GetPosition3fY(iStart),
                kMesh.VBuffer.GetPosition3fZ(iStart),
                            iStart );
        kNewVert.add(new Vector4f(kNewPoint4));

        /* Add "unknown" to the Left and Right edgelists for iStart point: */
        kNewLeft.add(new Integer(-1));
        kNewRight.add(new Integer(-1));


        kLeft.clear();
        kRight.clear();
        kMiddle.clear();

        kLeft = kNewLeft;
        kRight = kNewRight;

        /* Once all the Edges that are crossed are known, this won't change
         * and we can iterate over it many times to smooth the polyline
         * further: */
        for (int j = 0; j < 10; j++) {

            for (int i = 1; i < (kNewVert.size() - 1); i++) {
                Vector4f newPoint1 = new Vector4f();
                Vector4f kStart = kNewVert.get(i - 1);
                Vector4f kEnd = kNewVert.get(i + 1);

                int iMiddle = kLeft.get(i).intValue();
                int iSide = kRight.get(i).intValue();

                if ((iMiddle != -1) && (iSide != -1)) {
                    findMin(kMesh, 
                            new Vector3f(kStart.X, kStart.Y, kStart.Z), iMiddle, iSide,
                            new Vector3f(kEnd.X, kEnd.Y, kEnd.Z), newPoint1);
                    kNewVert.get(i).copy( newPoint1 );

                    newPoint1 = null;
                }
            }
        }

        iPath = 1;

        while (iPath < (kNewVert.size() - 1)) {
            boolean bRemoved = false;
            int iCurrentPoint = (int) kNewVert.get(iPath).W;

            if (iCurrentPoint == -1) {
                int iPrevPoint = (int) kNewVert.get(iPath - 1).W;
                int iNextPoint = (int) kNewVert.get(iPath + 1).W;

                if ((kLeft.get(iPath).intValue() == iPrevPoint) ||
                        (kRight.get(iPath).intValue() == iPrevPoint)) {
                    kNewVert.remove(iPath);
                    kLeft.remove(iPath);
                    kRight.remove(iPath);
                    bRemoved = true;
                } else if ((kLeft.get(iPath).intValue() == iNextPoint) ||
                               (kRight.get(iPath).intValue() == iNextPoint)) {
                    kNewVert.remove(iPath);
                    kLeft.remove(iPath);
                    kRight.remove(iPath);
                    bRemoved = true;
                }

            }

            if (!bRemoved) {
                iPath++;
            }
        }


        /* Smoothed Path Statistics: */
        m_fSmoothedPathLength = 0;

        /* Copy the new polyline into the m_kGeodesicVertices data member: */
        m_iNumGeodesicVertices = kNewVert.size();
        m_kGeodesicVertices = new Vector3f[m_iNumGeodesicVertices];

        for (int iVertex = 0; iVertex < m_iNumGeodesicVertices; iVertex++) {
            m_kGeodesicVertices[iVertex] = new Vector3f(kNewVert.get(iVertex).X,
                                                       kNewVert.get(iVertex).Y,
                                                        kNewVert.get(iVertex).Z);

            if (iVertex > 0) {
                m_fSmoothedPathLength += distance(m_kGeodesicVertices[iVertex], m_kGeodesicVertices[iVertex - 1]);
            }
        }

        /* Add the new path segment to the m_kGeodesic_Working list: */
        m_kGeodesic_Working.add(m_iNumWorking, kNewVert);
        m_kGeodesic_Working_Left.add(m_iNumWorking, kLeft);
        m_kGeodesic_Working_Right.add(m_iNumWorking, kRight);
        m_iNumWorking++;

        /* Delete temporary variables */
        kEndPoints = null;

        return true;
    }

    /**
     * Cut the m_kModified mesh.
     */
    public void cut() {

        if (m_kModified == null) {
            return;
        }

        m_kNewVerts = new LinkedList<Vector3f>();
        m_kNewNormals = new LinkedList<Vector3f>();
        m_kNewTexCoords = new LinkedList<Vector3f>();
        m_kNewColors = new LinkedList<ColorRGBA>();
        m_kNewTriangles = new LinkedList<Vector3f>();
        m_kRemoveTriangles = new LinkedList<Vector3f>();

        /* Backup the last finished mesh so it can be restored if the cut is
         * undone: */
        m_kLastCut = m_kLastFinished;

        /* Create the edgeList data for this mesh: */
        if (!createEdgeLists(m_kModified)) {
            MipavUtil.displayError("Mesh Cut failed 1, clearing cut");
            clearCut(false);

            return;
        }

        int iVertexCount = m_kModified.VBuffer.GetVertexQuantity();
        int iOldVertexCount = m_kModified.VBuffer.GetVertexQuantity();

        LinkedList<LinkedList<Integer>> kGeodesic_Separate = new LinkedList<LinkedList<Integer>>();

        /* First, "unzip" all open paths & remove: */
        if (m_bOpen == true) {
            LinkedList<Vector4f> kOpenPath = m_kGeodesic_Finished.get(0);
            iVertexCount += unZip(m_kModified, kOpenPath, iVertexCount, true, null);
        } else {

            /* Second, "unzip" all closed paths, storing the new path vertices: */
            LinkedList<Vector4f> kClosedPath = m_kGeodesic_Finished.get(0);
            LinkedList<Integer> kNewClosedPath = new LinkedList<Integer>();
            iVertexCount += unZip(m_kModified, kClosedPath, iVertexCount, false, kNewClosedPath);
            kGeodesic_Separate.add(kNewClosedPath);
        }

        //System.err.println( "Cut calling creatNewMesh 1 " + iVertexCount + " " + iOldVertexCount );
        TriMesh kCutMesh = createNewMesh(m_kModified, iVertexCount, iOldVertexCount);

        if (!createEdgeLists(kCutMesh)) {
            MipavUtil.displayError("Mesh Cut failed 2, clearing cut");
            clearCut(false);

            return;
        }

        if (kGeodesic_Separate.size() > 0) {
            m_kNewVerts.clear();
            m_kNewNormals.clear();
            m_kNewTexCoords.clear();
            m_kNewColors.clear();
            m_kNewTriangles.clear();

            iVertexCount = kCutMesh.VBuffer.GetVertexQuantity();
            iOldVertexCount = iVertexCount;
            m_aiIndexShift = null;
            m_abRemoveTris = null;

            int iNumVertsRemoved = findNewMeshes(kCutMesh, kGeodesic_Separate);

//             System.err.println( "Cut mesh num verts: " + kCutMesh.VBuffer.GetVertexQuantity() +
//                                 " num removed " + iNumVertsRemoved );
                                
            if (iNumVertsRemoved < m_iVertexCount) {
                iVertexCount -= iNumVertsRemoved;
//                 System.err.println( "Cut calling creatNewMesh 2 " + iVertexCount + " " + iOldVertexCount );
                kCutMesh = createNewMesh(kCutMesh, iVertexCount, iOldVertexCount);
            } else {
                MipavUtil.displayError("Mesh Cut failed 3, clearing cut");
                clearCut(false);

                return;
            }

            m_aiIndexShift = null;
            m_abRemoveTris = null;

            if (kCutMesh == null) {
                MipavUtil.displayError("Mesh Cut failed 4, clearing cut");
                clearCut(false);

                return;
            }
        }

        kCutMesh.SetName( m_kModified.GetName() );
        /* Delete m_kModified, and set to kCutMesh: */
        m_kModified = null;
        m_kModified = kCutMesh;

        /* Save a reference to the last finished mesh: */
        m_kLastFinished = m_kFinished;

        /* Store a reference to the current finished mesh: */
        m_kFinished = m_kModified;

        /* Export the modified original and the new meshes to the
         * surRenderer: */
        if (m_kPanel != null) {

            /* Backup the unmodified mesh: */
            m_kSurfaceBackup = null;
            m_kSurfaceBackup = new TriMesh(m_kSurface );
            m_kSurfaceBackup.SetName( m_kSurface.GetName() );
            m_kPanel.replaceMesh(m_kSurface, m_kModified);
            m_kSurface = null;
            m_kSurface = m_kModified;
            m_kStartSurface = m_kModified;
        }


        /* Delete local vars: */
        m_kNewVerts.clear();
        m_kNewVerts = null;
        m_kNewNormals.clear();
        m_kNewNormals = null;
        m_kNewTexCoords.clear();
        m_kNewTexCoords = null;
        m_kNewColors.clear();
        m_kNewColors = null;
        m_kNewTriangles.clear();
        m_kNewTriangles = null;
        m_kRemoveTriangles.clear();
        m_kRemoveTriangles = null;

        cleanUp();

        resetDrawGeodesic();
    }

    /**
     * Deletes all member variables, clean memory.
     */
    public void dispose() {

        /* delete member variables: */
        m_kPickColors[0] = null;
        m_kPickColors[1] = null;
        m_kPickColors = null;

        m_kPreviousStartPoint = null;

        clear(true);
        cleanUp();

        m_kStartPoint = null;
        m_kEndPoint = null;

        m_aiStartIndex = null;
        m_aiEndIndex = null;

        m_kPreviousStartPoint = null;
        m_aiPreviousStartIndex = null;

        m_kGeodesic_Working.clear();
        m_kGeodesic_Working = null;
        m_kGeodesic_Finished.clear();
        m_kGeodesic_Finished = null;

        m_kStartEndList.clear();
        m_kStartEndList = null;
        m_kOriginal = null;
        m_kLastFinished = null;
        m_kFinished = null;
        m_kLastCut = null;
    }


    /**
     * drawPath draws the Dijkstra path and Euclidian paths and adds them to the corresponding m_kDijkstraGeodesicGroup
     * and m_kEuclidianGeodesicGroups.
     *
     * @param  iStart  int Dijkstra path starting point
     * @param  iEnd    int Dijkstra path ending point
     */
    public void drawDijkstraEuclidianPath( TriMesh kParent, TriMesh kWorking, int iStart, int iEnd) {

        /* Check that the line exists, this is more important in livewire
         * mode, where the mouse motion may cause the start and end points to
         * be the same: */
        if (iStart == iEnd) {
            return;
        }

//         if (m_bLivewire && !m_bFirstWire) {
//             m_kDijkstraGeodesicGroup.removeChild(m_kDijkstraGeodesicGroup.numChildren() - 1);
//             m_kEuclidianGeodesicGroup.removeChild(m_kEuclidianGeodesicGroup.numChildren() - 1);
//         }

        if (m_bEndpointChanged) {
            m_kPanel.removeGeodesic( kParent, m_iDijkstraGeodesicGroup - 1, 1);
            m_kPanel.removeGeodesic( kParent, m_iEuclidianGeodesicGroup - 1, 2);
            m_iDijkstraGeodesicGroup--;
            m_iEuclidianGeodesicGroup--;
            drawDijkstraEuclidianPoint(kParent, m_kEndPoint, m_kPickColors[m_iNumPicked % 2]);
        }

        /* Recreate and Display Dijkstra's Path: */
        /* Cound the original mesh points, different from the smoothed mesh
         * points: */
        int iNode = iEnd;
        int iCount = 1;
        m_fDijkstraPathLength = 0;

        while (iNode != iStart) {

            if (iNode != iStart) {
                m_fDijkstraPathLength += distance(kWorking.VBuffer.GetPosition3(iNode),
                        kWorking.VBuffer.GetPosition3(m_iPrevious[iNode]));
            }

            iNode = m_iPrevious[iNode];
            iCount++;
        }

        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0, 3);
        VertexBuffer kVBuffer = new VertexBuffer( kAttr, iCount );
        ColorRGB kColor = new ColorRGB(1f,1f,0f);

        /* Add points to the line array: */
        /* Copy the mesh points into the LineArray: */
        iNode = iEnd;
        iCount = 0;
        kVBuffer.SetPosition3( iCount, kWorking.VBuffer.GetPosition3(iNode));
        kVBuffer.SetColor3( 0, iCount, kColor );
        iCount++;
        while (iNode != iStart) {
            iNode = m_iPrevious[iNode];
            kVBuffer.SetPosition3( iCount, kWorking.VBuffer.GetPosition3(iNode));
            kVBuffer.SetColor3( 0, iCount, kColor );
            iCount++;
        }
        Polyline kLine = new Polyline( kVBuffer, false, true );

        m_iDijkstraGeodesicGroup++;
        m_kPanel.addGeodesic( kParent, kLine, 1 );


        kVBuffer = new VertexBuffer( kAttr, 2 );
        kColor = new ColorRGB(0f, 1f, 1f);
        kVBuffer.SetPosition3(0, kWorking.VBuffer.GetPosition3(iStart));
        kVBuffer.SetColor3(0, 0, kColor);
        kVBuffer.SetPosition3(1, kWorking.VBuffer.GetPosition3(iEnd));
        kVBuffer.SetColor3(0, 1, kColor);
        kLine = new Polyline( kVBuffer, false, true );

        m_iEuclidianGeodesicGroup++;
        m_kPanel.addGeodesic( kParent, kLine, 2 );

        /* Update the panel display with the line distance information for all
         * three types of lines, geodesic, dijkstra, and euclidian: */
        if (m_kPanel != null) {
            float fEuclidian = distance(kWorking.VBuffer.GetPosition3(iStart),
                    kWorking.VBuffer.GetPosition3(iEnd));
            m_kPanel.setEuclidian(fEuclidian);
            m_kPanel.setGeodesicSmooth(m_fSmoothedPathLength);
            m_kPanel.setDijkstra(m_fDijkstraPathLength);
            m_kPanel.enableClearLast(true);
        }
    }

    /**
     * drawPath draws the Geodesic path as a LineArray and adds it to the children of the m_kSmoothedGeodesicGroup
     * object, it also draws the Dijkstra path and Euclidian paths and adds them to the corresponding
     * m_kDijkstraGeodesicGroup and m_kEuclidianGeodesicGroups.
     *
     * @param  iStart  int Geodesic path starting point
     * @param  iEnd    int Geodesic path ending point
     */
    public void drawGeodesicPath(TriMesh kParent, int iStart, int iEnd) {

        /* Check that the line exists, this is more important in livewire
         * mode, where the mouse motion may cause the start and end points to
         * be the same: */
        if (iStart == iEnd) {
            return;
        }

        if (m_bEndpointChanged) {

            m_kPanel.removeGeodesic( kParent, m_iSmoothedGeodesicGroup - 1, 0);
            m_iSmoothedGeodesicGroup--;
            drawGeodesicPoint(kParent, m_kEndPoint, m_kPickColors[m_iNumPicked % 2]);
        }

        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0, 3);
        VertexBuffer kVBuffer = new VertexBuffer( kAttr, m_iNumGeodesicVertices );
        ColorRGB kColor = new ColorRGB(1f,0f,0f);

        /* Add points to the line array: */
        for (int i = 0; i < m_iNumGeodesicVertices; i++) {
            kVBuffer.SetPosition3( i, m_kGeodesicVertices[i]);
            kVBuffer.SetColor3( 0, i, kColor );
        }
        Polyline kLine = new Polyline( kVBuffer, false, true );

        m_iSmoothedGeodesicGroup++;
        m_kPanel.addGeodesic( kParent, kLine, 0 );
    }

    /**
     * drawPath draws the Geodesic path as a LineArray and adds it to the children of the m_kSmoothedGeodesicGroup
     * object, it also draws the Dijkstra path and Euclidian paths and adds them to the corresponding
     * m_kDijkstraGeodesicGroup and m_kEuclidianGeodesicGroups.
     *
     * @param  iStart  int Geodesic path starting point
     * @param  iEnd    int Geodesic path ending point
     */
    public void drawPath( TriMesh kMesh, int iStart, int iEnd) {
        drawGeodesicPath(kMesh, iStart, iEnd);
        drawDijkstraEuclidianPath(kMesh, m_kModified, iStart, iEnd);
        //m_kSurface = kMesh;
    }


    /**
     * Closes the geodesic curve.
     *
     * @param  bOpen  bOpen: when true leaves the geodesic open, when false closes the geodesic by connecting to the
     *                first point on the polyline sequence.
     */
    public void finish(boolean bOpen) {

        if ((m_bFinished == true) || (m_kFirstPoint == null)) {
            return;
        } 
        m_bFinished = true;

        if (m_bLastWire) {
            m_bLastWire = false;

            for (int iPath = 0; iPath < m_kStartEndList.size(); iPath++) {
                getStartEnd(iPath);

                /* Compute the Geodesic curve. The 0.99
                 * parameter is an optimization to the
                 * Dijkstra search. */
                if (computeGeodesic(0.99f, true)) {

                    /* Draw the geodesic path */
                    drawGeodesicPath(m_kSurface, m_iStart, m_iEnd);

                    /* Triangulate path and export new mesh: */
                    triangulateMeshPath();

                    if (iPath == 0) {
                        drawGeodesicPoint(m_kSurface,
                                m_kModified.VBuffer.GetPosition3(m_iEnd), m_kPickColors[1]);
                    }

                    drawGeodesicPoint(m_kSurface,
                            m_kModified.VBuffer.GetPosition3(m_iStart), m_kPickColors[0]);

                }

                /* Delete the temporary variables used to
                 * calculate & draw the path */
                cleanUp();
            }

            m_kStartEndList.clear();
        }

        /* close the path: */
        if (bOpen == false) {
            setEndPoint(m_kStartPoint);
            setEndIndices(m_aiStartIndex);

            setStartPoint(m_kFirstPoint, false);
            setStartIndices(m_aiFirstIndex, false);

            /* Check that the two end points are not the
             * same: */
            if (!m_kStartPoint.isEqual(m_kEndPoint)) {

                /* Compute the Geodesic curve. The 0.99
                 * parameter is an optimization to the
                 * Dijkstra search. */
                if (computeGeodesic(0.99f, true)) {

                    /* Draw the geodesic path */
                    drawPath(m_kSurface, m_iStart, m_iEnd);

                    /* Triangulate path and export new mesh: */
                    triangulateMeshPath();
                }
                /* Delete the temporary variables used to
                 * calculate & draw the path */
                cleanUp();
            }
        }

        
        m_iLineClosed = m_iSmoothedGeodesicGroup - 1;
        m_iNumPicked = 0;

        /* Disable clearing the last added point: */
        if (m_kPanel != null) {
            m_kPanel.enableClearLast(false);
            m_kPanel.enableCut(true);
        }

        /* Add the working lists to the _Open or _Closed lists: */
        finishWorkingLists(bOpen);
        
        /* Save a reference to the last finished mesh: */
        m_kLastFinished = m_kFinished;

        /* Store a reference to the current finished mesh: */
        m_kFinished = m_kModified;
    }

    /**
     * Access on when picking with the mouse is enabled.
     *
     * @return  boolean picking is enabled or not.
     */
    public boolean getEnable() {
        return m_bEnabled;
    }


    /**
     * Returns the number of points in the geodesic curve.
     *
     * @return  int, the number of points on the Geodesic.
     */
    public int getNumPathPoints() {
        return m_iNumGeodesicVertices;
    }

    /**
     * Access to the i-th point on the Geodesic curve. All points on the geodesic curve lie on the triangle mesh.
     *
     * @param  iPoint  i-th point index
     * @param  kPoint  Vector3f point's coordinate
     */
    public void getPathPoint(int iPoint, Vector3f kPoint) {

        if ((iPoint >= 0) && (iPoint < m_iNumGeodesicVertices)) {
            kPoint.copy( m_kGeodesicVertices[iPoint] );
        }
    }

    /**
     * Access to the all the points on the Geodesic curve. All points on the geodesic curve lie on the triangle mesh.
     *
     * @param  akPoints  Vector3f[] points coordinates array
     */
    public void getPathPoints(Vector3f[] akPoints) {

        for (int iPoint = 0; iPoint < m_iNumGeodesicVertices; iPoint++) {
            akPoints[iPoint].copy( m_kGeodesicVertices[iPoint] );
        }
    }

    /**
     * Enables picking with the mouse and drawing the curve on the mesh.
     *
     * @param  bEnable  set the mouse picking enabled or not.
     */
    public void setEnable(boolean bEnable) {

        if (!bEnable && m_bEnabled) {
            finish(true);
        }

        m_bEnabled = bEnable;

        m_iNumPicked = 0;

        if ((m_bEnabled == false) && (m_iSmoothedGeodesicGroup > 0)) {
            m_iLineClosed = m_iSmoothedGeodesicGroup - 1;
        }
    }

    /**
     * Set the index of the vertex in the triangle mesh where the Geodesic curve is to end. The index must be less than
     * or equal to the number of vertices in the triangle mesh.
     *
     * @param  iIndex  int the index of the vertex in the triangle mesh where the Geodesic curve is to end.
     */
    public void setEndIndex(int iIndex) {

        if ((iIndex >= 0) && (iIndex < m_iVertexCount)) {
            m_iEnd = iIndex;
        }
    }

    /**
     * Sets the indices of the triangle that the end point is located in. This is used when the end point falls on the
     * inside of a triangle in the mesh, the indices parameter defines which triangle the end point falls in.
     *
     * @param  iIndices  int[3] array of vertex indices defining the end triangle.
     */
    public void setEndIndices(int[] iIndices) {

        if (m_aiEndIndex == null) {
            m_aiEndIndex = new int[3];
        }

        m_aiEndIndex[0] = iIndices[0];
        m_aiEndIndex[1] = iIndices[1];
        m_aiEndIndex[2] = iIndices[2];
    }

    /**
     * Sets the end point of the geodesic curve on the mesh. The point coordinates must be in local mesh coordinates.
     *
     * @param  kPoint  the point on the triangle mesh where the geodesic curve is to end, in mesh coordinates.
     */
    public void setEndPoint(Vector3f kPoint) {

        if (m_kEndPoint != null) {
            m_kEndPoint = null;
        }

        m_kEndPoint = new Vector3f(kPoint);
    }

    /**
     * Access function to set the Group object m_kGeodesicGroup. This is necessary for the Geodesic object to draw the
     * geodesic curve on the mesh. The curve may be drawn in one of three ways: (1) the straight-line Euclidian curve,
     * which is not constrained to lie on the mesh surface, or (2) Dijkstra's path, which falls along the original mesh
     * triangle edges, or (3) the Smoothed Geodesic, which is constrained to lie on the mesh surface, but which may
     * cross triangle edges. All three display modes are represented by a different BranchGroup.
     *
     * @param  kGeodesicGroup  Geodesic image scene graph node.
     */
//     public void setGeodesicGroup(Group kGeodesicGroup) {

//         if (m_kSmoothedGeodesicGroup == null) {
//             m_kSmoothedGeodesicGroup = new BranchGroup();
//             m_kSmoothedGeodesicGroup.setCapability(BranchGroup.ALLOW_DETACH);
//             m_kSmoothedGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND);
//             m_kSmoothedGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);
//             m_kSmoothedGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_READ);
//         }

//         if (m_kDijkstraGeodesicGroup == null) {
//             m_kDijkstraGeodesicGroup = new BranchGroup();
//             m_kDijkstraGeodesicGroup.setCapability(BranchGroup.ALLOW_DETACH);
//             m_kDijkstraGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND);
//             m_kDijkstraGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);
//             m_kDijkstraGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_READ);
//         }

//         if (m_kEuclidianGeodesicGroup == null) {
//             m_kEuclidianGeodesicGroup = new BranchGroup();
//             m_kEuclidianGeodesicGroup.setCapability(BranchGroup.ALLOW_DETACH);
//             m_kEuclidianGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND);
//             m_kEuclidianGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);
//             m_kEuclidianGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_READ);
//         }

//         if (m_kSwitchDisplay == null) {
//             m_kSwitchDisplay = new Switch();
//             m_kSwitchDisplay.setCapability(Switch.ALLOW_SWITCH_READ);
//             m_kSwitchDisplay.setCapability(Switch.ALLOW_SWITCH_WRITE);
//             m_kSwitchDisplay.setWhichChild(Switch.CHILD_ALL);
//             m_kSwitchDisplay.addChild(m_kSmoothedGeodesicGroup);
//             m_kSwitchDisplay.addChild(m_kDijkstraGeodesicGroup);
//             m_kSwitchDisplay.addChild(m_kEuclidianGeodesicGroup);
//             m_kSwitchDisplay.setWhichChild(m_iWhich);
//         }

//         if (m_kGeodesicGroup == null) {
//             m_kGeodesicGroup = kGeodesicGroup;

//             BranchGroup kBranchG = new BranchGroup();
//             kBranchG.addChild(m_kSwitchDisplay);
//             m_kGeodesicGroup.addChild(kBranchG);
//         }
//     }

    /**
     * Access to the JPanelGeodesic interface object.
     *
     * @param  kPanel  JPanelGeodesic geodesic panel
     */
    public void setPanel(JPanelGeodesic_WM kPanel) {
        m_kPanel = kPanel;
    }


//     /**
//      * Access function to set the pickCanvas. This is necessary for the Geodesic class to do picking with the mouse.
//      *
//      * @param  kPickCanvas  PickCanvas
//      */
//     public void setPickCanvas(PickCanvas kPickCanvas) {
//         m_kPickCanvas = kPickCanvas;
//         m_kPickCanvas.getCanvas().addMouseListener(this);
//         m_kPickCanvas.getCanvas().addMouseMotionListener(this);
//         m_kPickCanvas.getCanvas().addKeyListener(this);
//     }


    public void setPickedPoint( PickRecord kPickPoint, TriMesh kMesh )
    {               
        if ( kMesh == null )
        {
            return;
        }
//         System.err.println( kPickPoint.B0 + " " + kPickPoint.B1 + " " + kPickPoint.B2 );
//         System.err.println( kMesh.VBuffer.GetPosition3fX( kPickPoint.iV0 ) + " " + 
//                 kMesh.VBuffer.GetPosition3fY( kPickPoint.iV0 ) + " " +
//                 kMesh.VBuffer.GetPosition3fZ( kPickPoint.iV0 ) );
//         System.err.println( kMesh.VBuffer.GetPosition3fX( kPickPoint.iV1 ) + " " + 
//                 kMesh.VBuffer.GetPosition3fY( kPickPoint.iV1 ) + " " +
//                 kMesh.VBuffer.GetPosition3fZ( kPickPoint.iV1 ) );
//         System.err.println( kMesh.VBuffer.GetPosition3fX( kPickPoint.iV2 ) + " " + 
//                 kMesh.VBuffer.GetPosition3fY( kPickPoint.iV2 ) + " " +
//                 kMesh.VBuffer.GetPosition3fZ( kPickPoint.iV2 ) );
                
        Vector3f kP0 = kMesh.VBuffer.GetPosition3( kPickPoint.iV0 );
        kP0.scale(kPickPoint.B0);
        Vector3f kP1 = kMesh.VBuffer.GetPosition3( kPickPoint.iV1 );
        kP1.scale( kPickPoint.B1);
        Vector3f kP2 = kMesh.VBuffer.GetPosition3( kPickPoint.iV2 );
        kP2.scale( kPickPoint.B2 );
        Vector3f kPoint = Vector3f.add( kP0, kP1 );
        kPoint.add( kP2 );
//         System.err.println( kPoint.X() + " " + kPoint.Y() + " " + kPoint.Z() );        
        
        /* Increment the number of picked points. */
        m_iNumPicked++;

        if (m_iNumPicked > 2) {
            m_iNumPicked = 2;
        }
        
        /* If this is the first point picked in the sequence, then
         * mark the point with a sphere */
        if (m_iNumPicked == 1) {
            setStartSurface(kMesh);

            //setStartPoint( kMesh.VBuffer.GetPosition3( kPickPoint.iV0 ), true);
            setStartPoint( kPoint, true);
            int[] aiStartIndex = new int[]{ kPickPoint.iV0, kPickPoint.iV1, kPickPoint.iV2 };
            setStartIndices(aiStartIndex, true);

            /* If this s livewire mode, only draw the point in
             * Dijkstra's and the Euclidian scene graphs: */
             if (m_bLivewire) {
                 drawDijkstraEuclidianPoint(kMesh, m_kStartPoint, m_kPickColors[m_iNumPicked % 2]);
             }
             /* Otherwise add the point to all three scene graphs:
              * SmoothedGeodesic, Dijkstra, and Euclidian: */
             else {
                 drawPoint(kMesh, m_kStartPoint, m_kPickColors[m_iNumPicked % 2]);
             }

            /* Set first wire to be true and last wire to be false: */
            m_bFirstWire = true;
            m_bLastWire = false;
//             System.err.println( "first picked" );
        }
        /* If this is the second point in the sequence, then draw the
         * point and calculate the geodesic curve connecting the two
         * points. */
        else if (m_iNumPicked == 2) {
            setEndSurface(kMesh);

            if (m_kStartSurface != m_kEndSurface) {
                m_iNumPicked--;
            } else {

                /* If this is livewire mode, then the start point is
                 * already set in the mouseMove function, calculate
                 * the geodesic from there: */
                if (m_bLivewire) {

                    if (!m_kStartPoint.isEqual(m_kEndPoint)) {

                        if (computeGeodesic( 0.99f, false)) {
                            saveStartEnd();
                            drawDijkstraEuclidianPath(kMesh, m_kModified, m_iStart, m_iEnd);
                            m_bFinished = false;
                            m_bFirstWire = true;
                            drawDijkstraEuclidianPoint(kMesh, m_kStartPoint, m_kPickColors[m_iNumPicked % 2]);
                        }
                    }

                    /* the previous start point is stored in the
                     * current endpoint by the mouseMove function: */
                    setPreviousStartPoint(m_kEndPoint);
                    setPreviousStartIndices(m_aiEndIndex);
                }
                /* Not livewire mode: */
                else {

                    /* save this start point: */
                    setPreviousStartPoint(m_kStartPoint);
                    setPreviousStartIndices(m_aiStartIndex);
                }

                /* Set the end point to the old start point: */
                setEndPoint(m_kStartPoint);
                setEndIndices(m_aiStartIndex);

                /* Set start point to the new picked point: */
                //setStartPoint( kMesh.VBuffer.GetPosition3( kPickPoint.iV0 ), false);
                setStartPoint( kPoint, false);
                int[] aiStartIndex = new int[]{ kPickPoint.iV0, kPickPoint.iV1, kPickPoint.iV2 };
                setStartIndices(aiStartIndex, false);

//                 System.err.println( "second picked" );
                
                /* Check that the two end points are not the
                 * same: */
                if (!m_kStartPoint.isEqual(m_kEndPoint)) {

                    if (!m_bLivewire) {

//                         System.err.println( "computing geodesic" );
                        
                        /* Compute the Geodesic curve. The 0.99
                         * parameter is an optimization to the
                         * Dijkstra search. */
                        if (computeGeodesic(0.99f, true)) {

//                             System.err.println( "computing geodesic done" );
                            
                            /* Draw the geodesic path */
                            drawPath(kMesh, m_iStart, m_iEnd);

//                             System.err.println( "triangulate mesh path" );
                            /* Triangulate path and export new mesh: */
                            triangulateMeshPath();

//                             System.err.println( "triangulate mesh path done" );
                            m_bFinished = false;

                            drawPoint(kMesh, m_kStartPoint, m_kPickColors[m_iNumPicked % 2]);
                        }
                    }
                }

                /* Delete the temporary variables used to
                 * calculate & draw the path */
                cleanUp();
            }
        }
    }
    
    /**
     * Sets the indices of the triangle that the previous start point is located in. This is used when the start point
     * falls on the inside of a triangle in the mesh, the indices parameter defines which triangle the start point falls
     * in.
     *
     * @param  iIndices  index coordinate
     */
    public void setPreviousStartIndices(int[] iIndices) {

        if (m_aiPreviousStartIndex == null) {
            m_aiPreviousStartIndex = new int[3];
        }

        m_aiPreviousStartIndex[0] = iIndices[0];
        m_aiPreviousStartIndex[1] = iIndices[1];
        m_aiPreviousStartIndex[2] = iIndices[2];
    }

    /**
     * Set the radius of the spheres used to mark the start and end points on the geodesic curve.
     *
     * @param  fRadius  the size of the marker sphere to be drawn on the mesh
     */
    public void setRadius(float fRadius) {
        m_fRadius = fRadius;
    }


    /**
     * Set the index of the vertex in the triangle mesh where the Geodesic curve is to start. The index must be less
     * than or equal to the number of vertices in the triangle mesh.
     *
     * @param  iIndex  int the index of the vertex in the triangle mesh where the Geodesic curve is to start.
     */
    public void setStartIndex(int iIndex) {

        if ((iIndex >= 0) && (iIndex < m_iVertexCount)) {
            m_iStart = iIndex;
        }
    }

    /**
     * Sets the indices of the triangle that the start point is located in. This is used when the start point falls on
     * the inside of a triangle in the mesh, the indices parameter defines which triangle the start point falls in.
     *
     * @param  iIndices  int[3] array of vertex indices defining the start triangle.
     * @param  bFirst    flag indicate the first indices defining the start triangle.
     */
    public void setStartIndices(int[] iIndices, boolean bFirst) {

        if (m_aiStartIndex == null) {
            m_aiStartIndex = new int[3];
        }

        m_aiStartIndex[0] = iIndices[0];
        m_aiStartIndex[1] = iIndices[1];
        m_aiStartIndex[2] = iIndices[2];

        if (bFirst == true) {

            if (m_aiFirstIndex == null) {
                m_aiFirstIndex = new int[3];
            }

            m_aiFirstIndex[0] = iIndices[0];
            m_aiFirstIndex[1] = iIndices[1];
            m_aiFirstIndex[2] = iIndices[2];
        }
    }

    /**
     * Sets the start point of the geodesic curve on the mesh. The point coordinates must be in local mesh coordinates.
     *
     * @param  kPoint  the point on the triangle mesh where the geodesic curve is to start, in mesh coordinates.
     * @param  bFirst  flag indicate the first indices defining the start triangle.
     */
    public void setStartPoint(Vector3f kPoint, boolean bFirst) {

        if (m_kStartPoint != null) {
            m_kStartPoint = null;
        }

        m_kStartPoint = new Vector3f(kPoint);

        if (bFirst == true) {
            m_kFirstPoint = new Vector3f(kPoint);
        }
    }

    /**
     * Toggle between livewire mode and point & click mode.
     */
    public void toggleLivewire() {
        m_bLivewire = !m_bLivewire;
    }

    /**
     * Add the edge to the EdgeList, check to make sure that edge has not already been added.
     *
     * @param  iEdgeIndex  edge index
     * @param  iNewEdge    int added edge index
     */
    private void addEdge(int iEdgeIndex, int iNewEdge) {

        if (iEdgeIndex == iNewEdge) {
            return;
        }

        Integer kNewEdge = new Integer(iNewEdge);

        if (!m_akEdgeList[iEdgeIndex].contains(kNewEdge)) {
            m_akEdgeList[iEdgeIndex].add(kNewEdge);
        } else {
            kNewEdge = null;
        }

        kNewEdge = new Integer(iEdgeIndex);

        if (!m_akEdgeList[iNewEdge].contains(kNewEdge)) {
            m_akEdgeList[iNewEdge].add(kNewEdge);
        } else {
            kNewEdge = null;
        }

    }


    /**
     * Given a point which is known to be inside a triangle, and that triangle, this function determines which edge, if
     * any, that point falls on.
     *
     * @param   kMesh       TriMesh surface
     * @param   kPoint      Vector3f
     * @param   aiTriIndex  int[]
     * @param   kNormal     Vector3f
     *
     * @return  int
     */
    private int checkOnEdge(TriMesh kMesh, Vector3f kPoint, int[] aiTriIndex,
                            Vector3f kNormal, Vector3f kTexCoord, ColorRGBA kColor )
    {
        int iReturn = -1;

        Vector3f kTri0 = new Vector3f();
        kMesh.VBuffer.GetPosition3(aiTriIndex[0], kTri0);

        Vector3f kTri1 = new Vector3f();
        kMesh.VBuffer.GetPosition3(aiTriIndex[1], kTri1);

        Vector3f kTri2 = new Vector3f();
        kMesh.VBuffer.GetPosition3(aiTriIndex[2], kTri2);

        Vector3f kEdge = Vector3f.sub( kTri0, kTri1 );
        kEdge.normalize();

        Vector3f kP_Edge = Vector3f.sub( kPoint, kTri1 );
        float fDot = kEdge.dot(kP_Edge);
        Vector3f kNewPoint = new Vector3f((fDot * kEdge.X) + kTri1.X,
                (fDot * kEdge.Y) + kTri1.Y,
                (fDot * kEdge.Z) + kTri1.Z);

        Vector3f kNewNormal;
        Vector3f kNewTexCoord;
        ColorRGBA kNewColor;

        if (kNewPoint.isEqual(kPoint)) {
            //if (kNewPoint.epsilonEquals(kPoint, m_fEpsilon)) {
            kPoint.copy(kNewPoint);

            kNewNormal = getNormal(kMesh, aiTriIndex[0], aiTriIndex[1]);
            kNormal.copy(kNewNormal);

            kNewTexCoord = getTexCoord(kMesh, aiTriIndex[0], aiTriIndex[1]);
            kTexCoord.copy(kNewTexCoord);

            kNewColor = getColor(kMesh, aiTriIndex[0], aiTriIndex[1]);
            kColor.Copy(kNewColor);
            iReturn = 2;
        }

        if (iReturn == -1) {
            kEdge = Vector3f.sub( kTri0, kTri2 );
            kEdge.normalize();
            kP_Edge = Vector3f.sub( kPoint, kTri2 );

            fDot = kEdge.dot(kP_Edge);
            kNewPoint = new Vector3f((fDot * kEdge.X) + kTri2.X, 
                    (fDot * kEdge.Y) + kTri2.Y, 
                    (fDot * kEdge.Z) + kTri2.Z);

            if (kNewPoint.isEqual(kPoint)) {
                //if (kNewPoint.epsilonEquals(kPoint, m_fEpsilon)) {
                kPoint.copy(kNewPoint);

                kNewNormal = getNormal(kMesh, aiTriIndex[0], aiTriIndex[2]);
                kNormal.copy(kNewNormal);

                kNewTexCoord = getTexCoord(kMesh, aiTriIndex[0], aiTriIndex[2]);
                kTexCoord.copy(kNewTexCoord);

                kNewColor = getColor(kMesh, aiTriIndex[0], aiTriIndex[2]);
                kColor.Copy(kNewColor);
                iReturn = 1;
            }
        }

        if (iReturn == -1) {
            kEdge = Vector3f.sub( kTri2, kTri1 );
            kEdge.normalize();
            kP_Edge = Vector3f.sub( kPoint, kTri1 );
            fDot = kEdge.dot(kP_Edge);
            kNewPoint = new Vector3f((fDot * kEdge.X) + kTri1.X,
                    (fDot * kEdge.Y) + kTri1.Y,
                    (fDot * kEdge.Z) + kTri1.Z);

            if (kNewPoint.isEqual(kPoint)) {
                //if (kNewPoint.epsilonEquals(kPoint, m_fEpsilon)) {
                kPoint.copy(kNewPoint);

                kNewNormal = getNormal(kMesh, aiTriIndex[2], aiTriIndex[1]);
                kNormal.copy(kNewNormal);

                kNewTexCoord = getTexCoord(kMesh, aiTriIndex[2], aiTriIndex[1]);
                kTexCoord.copy(kNewTexCoord);

                kNewColor = getColor(kMesh, aiTriIndex[2], aiTriIndex[1]);
                kColor.Copy(kNewColor);
                iReturn = 0;
            }
        }

        kTri0 = null;
        kTri1 = null;
        kTri2 = null;
        kEdge = null;
        kP_Edge = null;

        if (iReturn == -1) {
            kNewNormal = getNormal(kMesh, aiTriIndex);
            kNormal.copy(kNewNormal);

            kNewTexCoord = getTexCoord(kMesh, aiTriIndex);
            kTexCoord.copy(kNewTexCoord);
            
            kNewColor = getColor(kMesh, aiTriIndex);
            kColor.Copy(kNewColor);
        }

        kNewNormal = null;
        kNewTexCoord = null;
        kNewColor = null;

        return iReturn;
    }

    /**
     * cleanUp deletes the data stuctures used for the Dijkstra's search, but does not delete the array of points on the
     * Geodesic curve.
     */
    private void cleanUp() {

        if (m_akEdgeList != null) {
            for (int i = 0; i < m_akEdgeList.length; i++) {
                m_akEdgeList[i].clear();
                m_akEdgeList[i] = null;
            }
            m_akEdgeList = null;
        }

        m_aiIndex = null;

        m_kBorder = null;

        m_fRemainingWeight = null;
        m_fWeight = null;
        m_bRelaxed = null;
        m_iPrevious = null;

        m_iStart = -1;
        m_iEnd = -1;
    }

    /**
     * clears the all the points added in livewire mode when clear all is called from the user interface.
     */
    private void clearAllStartEnd() {
        m_kStartEndList.clear();
    }

    /**
     * clears the last points added in livewire mode when clear last point is called from the user interface.
     */
    private void clearLastStartEnd() {
        m_kStartEndList.removeLast();
    }

    /**
     * Contains determines if the linked list kTriList contains the input triangle, which is specified by three vertex
     * indices. The LinkedList member function contains is not used because the indices may be in a different order, and
     * the function must return true if any of the triangles in the list match the input triangle regardless of the
     * order the vertices are specified
     *
     * @param   kTriList  Link List
     * @param   kNewTri   Point3i input triangle vertex indices
     *
     * @return  boolean contains the triangle or not.
     */
    private boolean contains(LinkedList kTriList, Vector3f kNewTri) {
        boolean bReturn = false;
        Vector3f kTri;

        for (int iNewTri = 0; iNewTri < kTriList.size(); iNewTri++) {
            kTri = (Vector3f) kTriList.get(iNewTri);

            if (triangleEquals( (int)kNewTri.X, (int)kNewTri.Y, (int)kNewTri.Z,
                    (int)kTri.X, (int)kTri.Y, (int)kTri.Z)) {
                bReturn = true;
            }
        }

        return bReturn;
    }
    
    /**
     * Return index of the Vector3f in the list.
     * @param kVecList list of Vector3f
     * @param kNewTri Vector3f
     * @return index of the Vector in the list, -1 if it is not in the list.
     */
    private int containsVector(LinkedList<Vector3f> kVecList, Vector3f kNewTri) {
        for (int i = 0; i < kVecList.size(); i++) {
            Vector3f kTri = kVecList.get(i);

            if ( kTri.isEqual(kNewTri))
            {
                return i;
            }
        }

        return -1;
    }

    /**
     * Create the edges list from the given surface triangle mesh.
     *
     * @param   kMesh surface
     *
     * @return  boolean success or not
     */
    @SuppressWarnings("unchecked")
    private boolean createEdgeLists(TriMesh kMesh) {
        if (m_akEdgeList != null) {
            for (int iEdge = 0; iEdge < m_akEdgeList.length; iEdge++) {
                m_akEdgeList[iEdge].clear();
            }
        }

        m_akEdgeList = null;

        if (m_aiIndex != null) {
            m_aiIndex = null;
        }

        m_iVertexCount = kMesh.VBuffer.GetVertexQuantity();
        m_akEdgeList = new LinkedList[m_iVertexCount];

        for (int iEdge = 0; iEdge < m_iVertexCount; iEdge++) {
            m_akEdgeList[iEdge] = new LinkedList<Integer>();
        }

        m_iIndexCount = kMesh.IBuffer.GetIndexQuantity();
        m_aiIndex = kMesh.IBuffer.GetData();

        int iNumTris = m_iIndexCount / 3;
        LinkedList<Integer> kEndPoints = new LinkedList<Integer>();
        boolean bReturn = true;

        for (int iTri = 0; iTri < iNumTris; iTri++) {
            int i0 = m_aiIndex[(iTri * 3) + 0];
            int i1 = m_aiIndex[(iTri * 3) + 1];
            int i2 = m_aiIndex[(iTri * 3) + 2];

//             if ( (i0 == i1) || (i0 == i2) || (i1 == i2) )
//             {
//                 System.err.println( "Bad tri: " + i0 + " " + i1 + " " + i2 );
//                 System.exit(0);
//             }
            
            addEdge(i0, i1);
            addEdge(i0, i2);
            addEdge(i1, i2);

            if (i0 != i1) {
                findTriPoints(i0, i1, kEndPoints);

                if ((kEndPoints.size() > 2) || (kEndPoints.size() == 0)) {
                    bReturn = false;
                }
            }

            if (i0 != i2) {
                findTriPoints(i0, i2, kEndPoints);

                if ((kEndPoints.size() > 2) || (kEndPoints.size() == 0)) {
                    bReturn = false;
                }
            }

            if (i1 != i2) {
                findTriPoints(i1, i2, kEndPoints);

                if ((kEndPoints.size() > 2) || (kEndPoints.size() == 0)) {
                    bReturn = false;
                }
            }
        }

        kEndPoints = null;

        return bReturn;
    }

    /**
     * creates a new mesh after triangulation or when a mesh is cut along the geodesic.
     *
     * @param   kMesh            TriMesh surface
     * @param   iVertexCount     int new vertex count
     * @param   iOldVertexCount  int old vertex cunt
     *
     * @return  TriMesh surface
     */
    private TriMesh createNewMesh(TriMesh kMesh, int iVertexCount, int iOldVertexCount) {

        Attributes kAttributes = new Attributes(kMesh.VBuffer.GetAttributes());
        VertexBuffer kVBuffer = new VertexBuffer( kAttributes, iVertexCount );

        int iIndex = 0;
        //System.err.println("CreateNewMesh 1");
        for (int i = 0; i < kMesh.VBuffer.GetVertexQuantity(); i++) {
            boolean bAdd = true;
            if (m_aiIndexShift != null) {

                if (m_aiIndexShift[i] == -1) {
                    bAdd = false;
                }
            }

            if (bAdd && (iIndex < iVertexCount)) {
                kVBuffer.SetPosition3( iIndex, kMesh.VBuffer.GetPosition3( i ) );
                kVBuffer.SetNormal3( iIndex, kMesh.VBuffer.GetNormal3( i ) );
                kVBuffer.SetColor4( 0, iIndex, kMesh.VBuffer.GetColor4( 0, i ) );
                kVBuffer.SetTCoord3( 0, iIndex, kMesh.VBuffer.GetTCoord3( 0, i ) );
                iIndex++;
            }
        }

        //System.err.println("CreateNewMesh 2");
        for (int i = 0; i < m_kNewVerts.size(); i++) {
            kVBuffer.SetPosition3( iIndex, m_kNewVerts.get(i));
            kVBuffer.SetNormal3( iIndex, m_kNewNormals.get(i));
            kVBuffer.SetColor4( 0, iIndex, m_kNewColors.get(i));
            kVBuffer.SetTCoord3( 0, iIndex, m_kNewTexCoords.get(i));
            iIndex++;
        }

        //System.err.println("CreateNewMesh 3");
        /* Get the old triangles: */
        int iIndexCount = kMesh.IBuffer.GetIndexQuantity();
        int[] aiIndex = kMesh.IBuffer.GetData();
        int iNumTris = iIndexCount / 3;

        /* remove m_kRemoveTriangles from the mesh: */
        Vector3f aiRemoveTri = new Vector3f();
        int iNumAdded = 0;
        int iNumRemoved = 0;

//         System.err.println("CreateNewMesh 4: " + iNumTris + " " + m_kRemoveTriangles.size() + " " + m_iNumRemoved + " " + (m_abRemoveTris != null));
//         if (m_abRemoveTris == null)
//         {
//             System.err.println( iNumTris - m_kRemoveTriangles.size() );
//         }
//         else
//         {
//             System.err.println( iNumTris - m_iNumRemoved );
//         }
        boolean[] abAdd = new boolean[iNumTris];
        for (int iTri = 0; iTri < iNumTris; iTri++) {
            boolean bAdd = true;

            if (m_abRemoveTris != null)
            {
                bAdd = !m_abRemoveTris[iTri];
            }
            else
            {
                for (int i = 0; i < m_kRemoveTriangles.size(); i++) {
                    aiRemoveTri.copy(m_kRemoveTriangles.get(i));

                    if (triangleEquals(aiIndex[(iTri * 3) + 0],
                                       aiIndex[(iTri * 3) + 1],
                                       aiIndex[(iTri * 3) + 2],
                                       (int)aiRemoveTri.X, (int)aiRemoveTri.Y, (int)aiRemoveTri.Z))
                    {
                        bAdd = false;
                        break;
                    }
                }
            }
            abAdd[iTri] = bAdd;

            if (bAdd) {
                iNumAdded++;
            } else {
                iNumRemoved++;
            }
        }

//         System.err.println("CreateNewMesh 5: " + iNumAdded + " new size = " + (iNumAdded + m_kNewTriangles.size()));
        int[] aiConnect = new int[(iNumAdded + m_kNewTriangles.size()) *3];
        iNumAdded = 0;
        for (int iTri = 0; iTri < iNumTris; iTri++)
        {
            if ( abAdd[iTri] )
            {
                if ((iVertexCount < iOldVertexCount) && (m_aiIndexShift != null)) {
                    int i0 = aiIndex[(iTri * 3) + 0];
                    int i1 = aiIndex[(iTri * 3) + 1];
                    int i2 = aiIndex[(iTri * 3) + 2];
                    i0 -= m_aiIndexShift[i0];
                    i1 -= m_aiIndexShift[i1];
                    i2 -= m_aiIndexShift[i2];
                    aiConnect[(iNumAdded * 3) + 0] = i0;
                    aiConnect[(iNumAdded * 3) + 1] = i1;
                    aiConnect[(iNumAdded * 3) + 2] = i2;
                } else {
                    aiConnect[(iNumAdded * 3) + 0] = aiIndex[(iTri * 3) + 0];
                    aiConnect[(iNumAdded * 3) + 1] = aiIndex[(iTri * 3) + 1];
                    aiConnect[(iNumAdded * 3) + 2] = aiIndex[(iTri * 3) + 2];
                }
                iNumAdded++;
            }
        }

//         System.err.println("CreateNewMesh 6: " + iNumAdded);
        /* Add new triangles to the mesh: */
        Vector3f aiAddTri = new Vector3f();
        for (int iTri = 0; iTri < m_kNewTriangles.size(); iTri++) {
            aiAddTri.copy(m_kNewTriangles.get(iTri));
            sortTriIndex(aiAddTri, kVBuffer);
            aiConnect[(iNumAdded * 3) + 0] = (int)aiAddTri.X;
            aiConnect[(iNumAdded * 3) + 1] = (int)aiAddTri.Y;
            aiConnect[(iNumAdded * 3) + 2] = (int)aiAddTri.Z;
            iNumAdded++;
        }

//         System.err.println("CreateNewMesh 7: " + iNumAdded );
        /* Delete local variables: */
        //aiIndex = null;

        //System.err.println("CreateNewMesh 8");
        IndexBuffer kIBuffer = new IndexBuffer(aiConnect.length, aiConnect);

        //System.err.println("CreateNewMesh 9");
        TriMesh kCutMesh = new TriMesh(kVBuffer, kIBuffer);

//         System.err.println("CreateNewMesh 10");
//         System.err.println( "Create new mesh: " + kCutMesh.VBuffer.GetVertexQuantity() + " " + 
//                             kCutMesh.IBuffer.GetIndexQuantity() );
        return kCutMesh;
    }

    /**
     * Calculate the Euclidean distance between two points.
     *
     * @param   kPoint1  Point3f starting point
     * @param   kPoint2  Point3f ending point
     *
     * @return  float distance
     */
    private float distance(Vector3f kPoint1, Vector3f kPoint2) {
        return (float) Math.sqrt(((kPoint1.X - kPoint2.X) * (kPoint1.X - kPoint2.X)) +
                                 ((kPoint1.Y - kPoint2.Y) * (kPoint1.Y - kPoint2.Y)) +
                                 ((kPoint1.Z - kPoint2.Z) * (kPoint1.Z - kPoint2.Z)));
    }

    /**
     * Draw the user-selected point as a sphere on the triangle mesh, the sphere is added to all three drawing groups:
     * m_kSmoothedGeodesicGroup, m_kDijkstraGeodesicGroup, and m_kEuclidianGeodesicGroup.
     *
     * @param  kStart  Point3f starting point
     * @param  kColor  Color3f ending point
     */
    private void drawDijkstraEuclidianPoint(TriMesh kParent, Vector3f kStart, ColorRGB kColor) {
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        StandardMesh kSM = new StandardMesh(kAttr);
        TriMesh kSphere = kSM.Sphere(64,64,m_fRadius);
        kSphere.Local.SetTranslate( kStart );

        MaterialState kMaterial = new MaterialState();
        kMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        kMaterial.Ambient = new ColorRGB(0.2f,0.2f,0.2f);
        kMaterial.Diffuse = new ColorRGB(kColor);
        kMaterial.Specular = new ColorRGB(0.2f,0.2f,0.2f);
        kMaterial.Shininess = 83.2f;
        kSphere.AttachGlobalState(kMaterial);

        /* Add to the Smoothed line: */
        m_bGroupAdded = true;
        m_iDijkstraGeodesicGroup++;
        m_iEuclidianGeodesicGroup++;
        m_kPanel.addGeodesic( kParent, kSphere, 1 );
        m_kPanel.addGeodesic( kParent, kSphere, 2 );
    }

    /**
     * Draw the user-selected point as a sphere on the triangle mesh, the sphere is added to all three drawing groups:
     * m_kSmoothedGeodesicGroup, m_kDijkstraGeodesicGroup, and m_kEuclidianGeodesicGroup.
     *
     * @param  kStart  Point3f starting point
     * @param  kColor  Color3f ending point
     */
    private void drawGeodesicPoint(TriMesh kParent, Vector3f kStart, ColorRGB kColor) {
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        StandardMesh kSM = new StandardMesh(kAttr);
        TriMesh kSphere = kSM.Sphere(64,64,m_fRadius);
        kSphere.Local.SetTranslate( kStart );

        MaterialState kMaterial = new MaterialState();
        kMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        kMaterial.Ambient = new ColorRGB(0.2f,0.2f,0.2f);
        kMaterial.Diffuse = new ColorRGB(kColor);
        kMaterial.Specular = new ColorRGB(0.2f,0.2f,0.2f);
        kMaterial.Shininess = 83.2f;
        kSphere.AttachGlobalState(kMaterial);

        /* Add to the Smoothed line: */
        m_iSmoothedGeodesicGroup++;
        m_bGroupAdded = true;
        
        m_kPanel.addGeodesic( kParent, kSphere, 0 );
    }

    /**
     * Draw the user-selected point as a sphere on the triangle mesh, the sphere is added to all three drawing groups:
     * m_kSmoothedGeodesicGroup, m_kDijkstraGeodesicGroup, and m_kEuclidianGeodesicGroup.
     *
     * @param  kStart  Point3f point coordinate
     * @param  kColor  Color3f point color
     */
    private void drawPoint( TriMesh kMesh, Vector3f kStart, ColorRGB kColor) {
        drawGeodesicPoint(kMesh, kStart, kColor);
        drawDijkstraEuclidianPoint(kMesh, kStart, kColor);
        //m_kSurface = kMesh;
    }

    /**
     * Used in cutting closed paths and creating new meshes to export to the scene graph.
     *
     * @param   kLoop  LinkedList surface mesh link list
     *
     * @return  int number of deleted vertices
     */
    private int extractNewMesh(LinkedList kLoop) {
        boolean[] bFound = new boolean[m_iVertexCount];
        m_aiIndexShift = new int[m_iVertexCount];

        for (int iVert = 0; iVert < m_iVertexCount; iVert++) {
            bFound[iVert] = false;
        }

        for (int iVert = 0; iVert < kLoop.size(); iVert++) {
            int iIndex = ((Integer) kLoop.get(iVert)).intValue();
            bFound[iIndex] = true;
            findEdges(iIndex, bFound);
        }

        int iShiftAmount = 0;
        int iNumDeletedVerts = 0;

        for (int iVert = 0; iVert < m_iVertexCount; iVert++) {

            if (bFound[iVert]) {
                m_aiIndexShift[iVert] = -1;
                iShiftAmount++;
                iNumDeletedVerts++;
            } else {
                m_aiIndexShift[iVert] = iShiftAmount;
            }
        }

        int iNumTris = m_iIndexCount / 3;
        m_abRemoveTris = new boolean[iNumTris];
        m_iNumRemoved = 0;
        m_iNumNotRemoved = 0;
        for (int iTri = 0; iTri < iNumTris; iTri++) {
            int i0 = m_aiIndex[(iTri * 3) + 0];
            int i1 = m_aiIndex[(iTri * 3) + 1];
            int i2 = m_aiIndex[(iTri * 3) + 2];

            if ((m_aiIndexShift[i0] == -1) && (m_aiIndexShift[i1] == -1) && (m_aiIndexShift[i2] == -1)) {
                m_abRemoveTris[iTri] = true;
                m_iNumRemoved++;
            } else {
                m_abRemoveTris[iTri] = false;
                m_iNumNotRemoved++;
            }
        }

        bFound = null;

        return iNumDeletedVerts;
    }

    /**
     * Go through the edge list find the the edge specified.
     *
     * @param  iIndex  int edge index
     * @param  bFound  boolean[] edge being found
     */
    private void findEdges(int iIndex, boolean[] bFound) {

        for (int iEdge = 0; iEdge < m_akEdgeList[iIndex].size(); iEdge++) {
            int iNextIndex =  m_akEdgeList[iIndex].get(iEdge).intValue();

            if (!bFound[iNextIndex]) {
                bFound[iNextIndex] = true;
                findEdges(iNextIndex, bFound);
            }
        }
    }


    /**
     * findMin finds the point (newpoint) along the edge that connects the points Side-Middle that minimizes the
     * distance Start-newpoint-End.
     *
     * @param   kStart      Point3f starting point
     * @param   iMiddle     int middle point
     * @param   iSide       int side point index
     * @param   kEnd        Point3f end index
     * @param   kNewPoint4  Point4f new point
     *
     * @return  float find min point index
     */
    private float findMin( TriMesh kMesh, Vector3f kStart, int iMiddle, int iSide, Vector3f kEnd, Vector4f kNewPoint4) {

        if ((iSide < 0) || (iSide >= m_iVertexCount) || (iMiddle < 0) || (iMiddle >= m_iVertexCount)) {
            System.err.println("findMin: error");
        }

        Vector3f kMiddle = kMesh.VBuffer.GetPosition3(iMiddle);
        Vector3f kSide = kMesh.VBuffer.GetPosition3(iSide);


        /* Setup the vector from Side to Middle, we will add a fraction of
         * this vector to Middle each time through the loop, creating a new
         * point and re-evaluate the new path. */
        Vector3f kDiff = Vector3f.sub( kSide, kMiddle );
        kDiff.scale( 1.0f/10.0f );

        /* fDistance is used the path length of the current path
         * Start-newpoint-End: */
        float fDistance;

        /* The first path is Start-Middle-End, find that distance: */
        Vector3f kPath = Vector3f.sub( kStart, kMiddle );

        fDistance = kPath.length();
        kPath = Vector3f.sub( kMiddle, kEnd );
        fDistance += kPath.length();

        float fMin1 = fDistance;
        Vector3f kNewPoint1 = new Vector3f(kMiddle);
        Vector3f kNewPoint = new Vector3f(kMiddle);

        /* Loop, adding 1/10 of the vector between Middle-Side to Middle and
         * reevaluate the path Start-newpoint-End, keep track of the point
         * that is the minimum path length: */
        int iMin = -1;

        for (int j = 0; j < 10; j++) {
            kNewPoint1.add( kDiff );

            kPath = Vector3f.sub( kStart, kNewPoint1 );
            fDistance = kPath.length();

            kPath = Vector3f.sub( kNewPoint1, kEnd );
            fDistance += kPath.length();

            if (fDistance < fMin1) {
                iMin = j;
                fMin1 = fDistance;
                kNewPoint.copy( kNewPoint1 );
            }
        }

        if (iMin == -1) {
            kNewPoint4.set( kMiddle.X, 
                                kMiddle.Y,
                                kMiddle.Z,
                                iMiddle );
        } else if (iMin == 9) {
            kNewPoint4.set( kSide.X,
                                kSide.Y,
                                kSide.Z,
                                iSide );
        } else {
            kNewPoint4.set( kNewPoint.X,
                                kNewPoint.Y,
                                kNewPoint.Z,
                                -1 );
        }

        /* delete temporary variables: */
        kPath = null;
        kDiff = null;
        kNewPoint1 = null;
        kNewPoint = null;

        return fMin1;
    }


    /**
     * Used in cutting closed paths and creating new meshes to export to the scene graph.
     *
     * @param   kSourceMesh             TriMesh surface mesh
     * @param   kGeodesic_Closed_Loops  LinkedList closed path
     *
     * @return  int
     */
    private int findNewMeshes(TriMesh kSourceMesh, LinkedList kGeodesic_Closed_Loops) {
        LinkedList kClosedLoop = (LinkedList) kGeodesic_Closed_Loops.get(0);
        int iNumDeletedVerts = extractNewMesh(kClosedLoop);

        if (iNumDeletedVerts < m_iVertexCount) {
            outputDeletedAsNew(kSourceMesh);
        }

        return iNumDeletedVerts;
    }


    /**
     * The findSmallest function searches through the list of vertices that are not yet relaxed but that have been
     * visited by the Dijkstra Search, so that the weight factor is not Float.MAX_VALUE, but also is not the minimum
     * value for that node. In Dijkstra's search the vertex with the smallest weight is relaxed first, a greedy
     * algorithm that chooses the locally closest vertex to add to the path. The final weight for the vertex is
     * determined when it is relaxed.
     *
     * <p>This function uses the data member m_kBorder -- the linked list of vertices that have been visited by
     * Dijkstra's search, but which have not yet been relaxed.</p>
     *
     * @return  int index of the vertex with the smallest weight
     */
    private int findSmallest() {

        /* Get the number of vertices so we can search the entire border
         * list:*/
        int iSize = m_kBorder.size();

        if (iSize == 0) {
            return -1;
        }

        int iVertex;
        int iSmallest = m_kBorder.get(0).intValue();

        /* Loop over all the vertices in the border: */
        for (int iBorder = 0; iBorder < iSize; iBorder++) {
            iVertex = m_kBorder.get(iBorder).intValue();

            /* If the weight of the vertex, plus the remaining weight factor
             * is less then the current smallest weight, set the smallest to
             * be that vertex: */
            if ((m_fWeight[iVertex] + m_fRemainingWeight[iVertex]) <
                    (m_fWeight[iSmallest] + m_fRemainingWeight[iSmallest])) {
                iSmallest = iVertex;
            }
        }

        /* Return the index of the vertex with the smallest weight: */
        return iSmallest;
    }

    /**
     * Find the next side triangle index.
     *
     * @param   iNode1      int node 1 along the side
     * @param   iNode2      int node 2 along the side
     * @param   kEndPoints  end point Link List
     *
     * @return  int next side triangle index
     */
    private int findTriPoints(int iNode1, int iNode2, LinkedList<Integer> kEndPoints) {
        kEndPoints.clear();

        boolean bAdd = true;
        int iNumTris = m_iIndexCount / 3;

        for (int iTri = 0; iTri < iNumTris; iTri++) {
            bAdd = true;

            int i0 = m_aiIndex[(iTri * 3) + 0];
            int i1 = m_aiIndex[(iTri * 3) + 1];
            int i2 = m_aiIndex[(iTri * 3) + 2];

            if (((i0 == iNode1) && (i1 == iNode2)) || ((i0 == iNode2) && (i1 == iNode1))) {

                for (int iEnd = 0; iEnd < kEndPoints.size(); iEnd++) {

                    if (kEndPoints.get(iEnd).intValue() == i2) {
                        bAdd = false;
                    }
                }

                if (bAdd) {
                    kEndPoints.add(new Integer(i2));
                }

            } else if (((i0 == iNode1) && (i2 == iNode2)) || ((i0 == iNode2) && (i2 == iNode1))) {

                for (int iEnd = 0; iEnd < kEndPoints.size(); iEnd++) {

                    if (kEndPoints.get(iEnd).intValue() == i1) {
                        bAdd = false;
                    }
                }

                if (bAdd) {
                    kEndPoints.add(new Integer(i1));
                }
            } else if (((i1 == iNode1) && (i2 == iNode2)) || ((i1 == iNode2) && (i2 == iNode1))) {

                for (int iEnd = 0; iEnd < kEndPoints.size(); iEnd++) {

                    if (kEndPoints.get(iEnd).intValue() == i0) {
                        bAdd = false;
                    }
                }

                if (bAdd) {
                    kEndPoints.add(new Integer(i0));
                }
            }
        }

        return kEndPoints.size();
    }


    /**
     * Adds the working paths to the finished path lists, combining the segments in the working paths into one path.
     *
     * @param  bOpen  boolean close path or not
     */
    private void finishWorkingLists(boolean bOpen) {

        /* New lists that will combine the path segments in the
         * m_kGeodesic_Working list: */
        LinkedList<Vector4f> kGeodesic;

        if (m_iNumWorking > 1) {
            kGeodesic = new LinkedList<Vector4f>();

            /* The total number of points in the Geodesic and Dijkstra's path,
             * for all segments: */
            int iGeodesicCount = 0;

            /* For each segment in the working paths: */
            for (int iList = 0; iList < m_iNumWorking; iList++) {

                /* Get the Geodesic segment: */
                LinkedList<Vector4f> kGeodesicSegment = m_kGeodesic_Working.get(iList);

                /* Add each vertex in the segment to the combined path: */
                for (int iVert = 0; iVert < kGeodesicSegment.size(); iVert++) {

                    /* The last vert of the previous list and the first vert
                     * of this list are the same, so if this isn't the first
                     * list (iList == 0), then only add when (iVert != 0): */
                    if ((iList == 0) || (iVert != 0)) {

                        if (!kGeodesic.contains(kGeodesicSegment.get(iVert)) ||
                                ((iList == (m_iNumWorking - 1)) && (iVert == (kGeodesicSegment.size() - 1)))) {
                            kGeodesic.add(iGeodesicCount, kGeodesicSegment.get(iVert));
                            iGeodesicCount++;
                        } else {
                            int iIndex = kGeodesic.indexOf(kGeodesicSegment.get(iVert));

                            for (int i = iIndex; i < iGeodesicCount; i++) {
                                kGeodesic.removeLast();
                            }

                            iGeodesicCount = iIndex;
                            kGeodesic.add(iGeodesicCount, kGeodesicSegment.get(iVert));
                            iGeodesicCount++;
                        }
                    }
                }
            }
        }
        /* Just one path segment, so copy directly, no need to combine: */
        else {
            kGeodesic = m_kGeodesic_Working.get(0);
        }

        /* Last vert in the last list: */
        /* If it's a closed loop: */
        if (kGeodesic.getFirst().isEqual(kGeodesic.getLast())) {
            bOpen = false;
        }

        /* Add working lists to the m_kGeodesic_Finished list: */
        m_bOpen = bOpen;
        m_kGeodesic_Finished.clear();
        m_kGeodesic_Finished.add(0, kGeodesic);

        m_iNumWorking = 0;
        m_kGeodesic_Working.clear();
        m_kGeodesic_Working_Left.clear();
        m_kGeodesic_Working_Right.clear();
    }

    /**
     * Get the the triangle color from the given triangle index.
     *
     * @param   kMesh    TriMesh surface mesh
     * @param   iIndex1  int triangle point index 1
     * @param   iIndex2  int triangle point index 2
     *
     * @return  interpolated color the triangle
     */
    private ColorRGBA getColor(TriMesh kMesh, int iIndex1, int iIndex2) {
        ColorRGBA kSide1 = new ColorRGBA();
        ColorRGBA kSide2 = new ColorRGBA();
        kMesh.VBuffer.GetColor4(0, iIndex1, kSide1);
        kMesh.VBuffer.GetColor4(0, iIndex2, kSide2);

        ColorRGBA kColor = new ColorRGBA();
        kColor.R = (kSide1.R + kSide2.R) / 2.0f;
        kColor.G = (kSide1.G + kSide2.G) / 2.0f;
        kColor.B = (kSide1.B + kSide2.B) / 2.0f;
        kColor.A = (kSide1.A + kSide2.A) / 2.0f;

        kSide1 = null;
        kSide2 = null;

        return kColor;
    }

    /**
     * Calculates and returns the start point color for a new starting point
     * inside an existing triangle. The new color is the average of the colors
     * at each point in the triangle the starting point is inside
     *
     * @param   kMesh    TriMesh surface mesh
     * @param   aiIndex  int[] 3 triangle points
     *
     * @return  Color4f Average color of the triangle.
     */
    private ColorRGBA getColor(TriMesh kMesh, int[] aiIndex) {

        ColorRGBA kColor0 = new ColorRGBA();
        kMesh.VBuffer.GetColor4(0, aiIndex[0], kColor0);

        ColorRGBA kColor1 = new ColorRGBA();
        kMesh.VBuffer.GetColor4(0, aiIndex[1], kColor1);

        ColorRGBA kColor2 = new ColorRGBA();
        kMesh.VBuffer.GetColor4(0, aiIndex[2], kColor2);
        
        ColorRGBA kColor = new ColorRGBA();
        kColor.R = (kColor0.R + kColor1.R + kColor2.R) / 3.0f;
        kColor.G = (kColor0.G + kColor1.G + kColor2.G) / 3.0f;
        kColor.B = (kColor0.B + kColor1.B + kColor2.B) / 3.0f;
        kColor.A = (kColor0.A + kColor1.A + kColor2.A) / 3.0f;

        return kColor;
    }

    /**
     * Get the the triangle normal from the given triangle index.
     *
     * @param   kMesh    TriMesh surface mesh
     * @param   iIndex1  int triangle point index 1
     * @param   iIndex2  int triangle point index 2
     *
     * @return  Vector3f normal of the triangle
     */
    private Vector3f getNormal(TriMesh kMesh, int iIndex1, int iIndex2) {
        Vector3f kSide1 = new Vector3f();
        Vector3f kSide2 = new Vector3f();
        kMesh.VBuffer.GetNormal3(iIndex1, kSide1);
        kMesh.VBuffer.GetNormal3(iIndex2, kSide2);

        Vector3f kNormal = Vector3f.add( kSide1, kSide2 );
        kNormal.scale(1.0f/2.0f).normalize();

        kSide1 = null;
        kSide2 = null;

        return kNormal;
    }


    /**
     * Calculates and returns the start point normal for a new starting point inside an existing triangle. The new
     * normal is the average of the normals at each point in the triangle the starting point is inside
     *
     * @param   kMesh    TriMesh surface mesh
     * @param   aiIndex  int[] 3 triangle points
     *
     * @return  Vector3f Average normal of the triangle.
     */
    private Vector3f getNormal(TriMesh kMesh, int[] aiIndex) {
        Vector3f kNormal0 = new Vector3f();
        kMesh.VBuffer.GetNormal3(aiIndex[0], kNormal0);

        Vector3f kNormal1 = new Vector3f();
        kMesh.VBuffer.GetNormal3(aiIndex[1], kNormal1);

        Vector3f kNormal2 = new Vector3f();
        kMesh.VBuffer.GetNormal3(aiIndex[2], kNormal2);

        Vector3f kNormal = Vector3f.add( kNormal0, kNormal1);
        kNormal.add(kNormal2).scale( 1.0f / 3.0f).normalize();

        return kNormal;
    }

    /**
     * getPathIndex returns what the new vertex index should be. If the input index, iIndex, does fall on the cut path,
     * kPath, then that point on the path is going to be duplicated to disconnect the triangles on either side of the
     * cut path. Therefore the input index needs to be translated into a new vertex index, based on the current
     * iVertexCount, the point's position along the path, and wehter or not the path is open or closed.
     *
     * @param   kPath         LinkedList path link list
     * @param   iVertexCount  int vertex count
     * @param   iIndex        int path index
     * @param   bOpen         boolean close path or not
     *
     * @return  int the new vertex index
     */
    private int getPathIndex(LinkedList kPath, int iVertexCount, int iIndex, boolean bOpen) {
        int iFirst = (int) ((Vector4f) kPath.get(0)).W;
        int iLast = (int) ((Vector4f) kPath.get(kPath.size() - 1)).W;

        for (int iPath = 0; iPath < kPath.size(); iPath++) {

            if (iIndex == ((Vector4f) kPath.get(iPath)).W) {

                if ((iIndex == iFirst) || (iIndex == iLast)) {

                    if (!bOpen) {
                        return iVertexCount + (kPath.size() - 2);
                    } 
                    return iIndex;
                } 
                return iVertexCount + (iPath - 1);
            }
        }

        return iIndex;
    }

    /**
     * In livewire mode the endpoints of each path segment along Dijkstra's curve -- between each of the user-selected
     * points -- are stored so that the smoothed geodesic may be calculated and displayed later. This function retrieves
     * the stored values when the livewire path is finished
     *
     * @param  iWhich  int path index
     */
    private void getStartEnd(int iWhich) {
        LinkedList kStartEndList = m_kStartEndList.get(iWhich);

        Vector3f kEndIndex = new Vector3f((Vector3f) kStartEndList.get(0));
        Vector3f kStartIndex = new Vector3f((Vector3f) kStartEndList.get(1));
        m_kEndPoint = new Vector3f((Vector3f) kStartEndList.get(2));
        m_kStartPoint = new Vector3f((Vector3f) kStartEndList.get(3));

        m_aiEndIndex[0] = (int)kEndIndex.X;
        m_aiEndIndex[1] = (int)kEndIndex.Y;
        m_aiEndIndex[2] = (int)kEndIndex.Z;

        m_aiStartIndex[0] = (int)kStartIndex.X;
        m_aiStartIndex[1] = (int)kStartIndex.Y;
        m_aiStartIndex[2] = (int)kStartIndex.Z;
    }


    /**
     * Get the the triangle texture coordinate from the given triangle index.
     *
     * @param   kMesh    TriMesh surface mesh
     * @param   iIndex1  int triangle point index 1
     * @param   iIndex2  int triangle point index 2
     *
     * @return  interpolated texture coordinate the triangle
     */
    private Vector3f getTexCoord(TriMesh kMesh, int iIndex1, int iIndex2) {
        Vector3f kSide1 = new Vector3f();
        Vector3f kSide2 = new Vector3f();
        kMesh.VBuffer.GetTCoord3(0, iIndex1, kSide1);
        kMesh.VBuffer.GetTCoord3(0, iIndex2, kSide2);

        Vector3f kTexCoord = Vector3f.add( kSide1, kSide2 );
        kTexCoord.scale(1.0f/2.0f);

        kSide1 = null;
        kSide2 = null;

        return kTexCoord;
    }


    /**
     * Calculates and returns the start point texture coordinate for a new
     * starting point inside an existing triangle. The new texture coordinate
     * is the average of the texture coordinates at each point in the triangle
     * the starting point is inside
     *
     * @param   kMesh    TriMesh surface mesh
     * @param   aiIndex  int[] 3 triangle points
     *
     * @return  Texture3f Average texture coordinate of the triangle.
     */
    private Vector3f getTexCoord(TriMesh kMesh, int[] aiIndex) {

        Vector3f kTexCoord0 = new Vector3f();
        kMesh.VBuffer.GetTCoord3(0, aiIndex[0], kTexCoord0);

        Vector3f kTexCoord1 = new Vector3f();
        kMesh.VBuffer.GetTCoord3(0, aiIndex[1], kTexCoord1);

        Vector3f kTexCoord2 = new Vector3f();
        kMesh.VBuffer.GetTCoord3(0, aiIndex[2], kTexCoord2);
        
        Vector3f kTexCoord = Vector3f.add( kTexCoord0, kTexCoord1 );
        kTexCoord.add(kTexCoord2).scale( 1.0f / 3.0f);
        return kTexCoord;
    }

    /**
     * Initializes the colors for the first point on the curve, and the successive points.
     */
    private void initColors() {
        m_kPickColors = new ColorRGB[2];
        m_kPickColors[0] = new ColorRGB(1, 0, 0);
        m_kPickColors[1] = new ColorRGB(1, 1, 0);
    }


    /**
     * initializeGeodesic copies the triangle mesh and initializes the data structures for Dijkstra's Single-Source
     * shortest path algorithm.
     *
     * <p>If the start and end vertices do not fall on a triangle vertex, then the triangle they fall inside split into
     * three new triangles, by connecting the new vertex to each of the triangle vertices.</p>
     *
     * @param  fPercentage  float the optimization parameter for Dijkstra's shortest-path search
     */
    @SuppressWarnings("unchecked")
    private void initializeGeodesic(float fPercentage) {

        /* Check that m_kStartPoint and m_kEndPoint are not already on the
         * mesh, otherwise they will be added and the triangles
         * re-triangulated: */
        boolean bFirstSegment = false;

        if (m_kEndPoint.isEqual(m_kFirstPoint)) {
            bFirstSegment = true;
        }

        m_bEndpointChanged = false;
        m_iVertexCount = m_kSurface.VBuffer.GetVertexQuantity();

        Vector3f kVertex = new Vector3f();

        for (int iVert = 0; iVert < m_iVertexCount; iVert++) {
            m_kSurface.VBuffer.GetPosition3(iVert, kVertex);

            if (m_kStartPoint.isEqual(kVertex)) {
                //if (m_kStartPoint.epsilonEquals(kVertex, m_fEpsilon)) {
                m_kStartPoint.copy(kVertex);
                m_iStart = iVert;
            }

            if (m_kEndPoint.isEqual(kVertex)) {
                //if (m_kEndPoint.epsilonEquals(kVertex, m_fEpsilon)) {

                if (m_kEndPoint.isEqual(m_kFirstPoint)) {
                    m_kFirstPoint.copy(kVertex);
                }

                m_bEndpointChanged = true;
                m_kEndPoint.copy(kVertex);
                m_iEnd = iVert;
            }
        }


        int iNumberNewVertices = 0;
        boolean bAddStart = false;
        boolean bAddEnd = false;

        /* If the start and end vertices do not fall on a triangle vertex,
         * then we add a new vertex to represent them: */
        if (m_iStart == -1) {
            iNumberNewVertices++;
            m_iVertexCount++;
            setStartIndex(m_iVertexCount - 1);
            bAddStart = true;
        }

        if (m_iEnd == -1) {
            iNumberNewVertices++;
            m_iVertexCount++;
            setEndIndex(m_iVertexCount - 1);
            bAddEnd = true;
        }

        if (bAddEnd && bAddStart) {

            if (triangleEquals(m_aiStartIndex[0], m_aiStartIndex[1], m_aiStartIndex[2], m_aiEndIndex[0],
                                   m_aiEndIndex[1], m_aiEndIndex[2])) {
                Vector3f kPoint = new Vector3f();
                m_kSurface.VBuffer.GetPosition3(m_aiEndIndex[0], kPoint);

                float fDistance0 = distance(m_kEndPoint, kPoint);

                m_kSurface.VBuffer.GetPosition3(m_aiEndIndex[1], kPoint);

                float fDistance1 = distance(m_kEndPoint, kPoint);

                m_kSurface.VBuffer.GetPosition3(m_aiEndIndex[2], kPoint);

                float fDistance2 = distance(m_kEndPoint, kPoint);
                float fMin = fDistance0;
                int iMin = 0;

                if (fDistance1 < fMin) {
                    iMin = 1;
                    fMin = fDistance1;
                }

                if (fDistance2 < fMin) {
                    iMin = 2;
                    fMin = fDistance2;
                }

                setEndIndex(m_aiEndIndex[iMin]);
                m_kSurface.VBuffer.GetPosition3(m_iEnd, m_kEndPoint);

                bAddEnd = false;
                iNumberNewVertices--;
                m_iVertexCount--;
            }
        }


        /* If start and/or end are new vertives, check if either falls on the
         * edge of the triangles, instead of in the center of the
         * triangle:  */
        boolean bStartOnEdge = false;
        boolean bEndOnEdge = false;
        int iStartEdge = -1;
        int iEndEdge = -1;
        Vector3f kStartNormal = new Vector3f();
        Vector3f kEndNormal = new Vector3f();

        Vector3f kStartTexCoord = new Vector3f();
        Vector3f kEndTexCoord = new Vector3f();

        ColorRGBA kStartColor = new ColorRGBA();
        ColorRGBA kEndColor = new ColorRGBA();

        if (bAddStart) {
            iStartEdge = checkOnEdge(m_kSurface, m_kStartPoint, m_aiStartIndex, kStartNormal, kStartTexCoord, kStartColor);

            if (iStartEdge != -1) {
                bStartOnEdge = true;
            }
        }

        if (bAddEnd) {
            iEndEdge = checkOnEdge(m_kSurface, m_kEndPoint, m_aiEndIndex, kEndNormal, kEndTexCoord, kEndColor);

            if (iEndEdge != -1) {
                bEndOnEdge = true;
            }
        }

        /* Allocate memory for the data structures: */
        VertexBuffer kVBuffer = new VertexBuffer( m_kSurface.VBuffer.GetAttributes(), m_iVertexCount );

        /* EdgeLists, one per vertex: */
        m_akEdgeList = new LinkedList[m_iVertexCount];

        /* The touched, but not relaxed vertices for Dijkstra's search: */
        m_kBorder = new LinkedList<Integer>();

        /* Optimization weight: straight-line distance from this vertex to end
         * vertex: */
        m_fRemainingWeight = new float[m_iVertexCount];

        /* Path length along edges from start vertex to this vertex:*/
        m_fWeight = new float[m_iVertexCount];

        /* Whether or not this vertex is relaxed: */
        m_bRelaxed = new boolean[m_iVertexCount];

        /* The vertex on the path before this one: */
        m_iPrevious = new int[m_iVertexCount];


        /* fDistance and kDiff are used to calculate the straight-line
         * distance from each vertex to the end vertex. */
        float fDistance;

        /* Copy the vertices and initialize the weights for Dijkstra's
         * search */
        for (int iVertex = 0; iVertex < m_iVertexCount; iVertex++) {

            /* Add the vertices and normals in the mesh: */
            if (iVertex < (m_iVertexCount - iNumberNewVertices)) {
                kVBuffer.SetPosition3( iVertex, m_kSurface.VBuffer.GetPosition3(iVertex) );
                kVBuffer.SetNormal3( iVertex, m_kSurface.VBuffer.GetNormal3(iVertex) );
                kVBuffer.SetTCoord3(0, iVertex, m_kSurface.VBuffer.GetTCoord3(0, iVertex) );
                kVBuffer.SetColor4( 0, iVertex, m_kSurface.VBuffer.GetColor4(0, iVertex) );
            }
            /* All the vertices have been added, if the number of new vertices
             * is 2, add both the start and end vertices: */
            else if (iNumberNewVertices == 2) {

                if (iVertex == (m_iVertexCount - 2)) {
                    kVBuffer.SetPosition3( iVertex, m_kStartPoint );
                    kVBuffer.SetNormal3( iVertex, kStartNormal );
                    kVBuffer.SetTCoord3( 0, iVertex, kStartTexCoord );
                    kVBuffer.SetColor4( 0, iVertex, kStartColor );
                } else if (iVertex == (m_iVertexCount - 1)) {
                    kVBuffer.SetPosition3( iVertex, m_kEndPoint );
                    kVBuffer.SetNormal3( iVertex, kEndNormal );
                    kVBuffer.SetTCoord3( 0, iVertex, kEndTexCoord );
                    kVBuffer.SetColor4( 0, iVertex, kEndColor );
                }
            }
            /* All the vertices have been added, if the number of new vertices
             * is 1, add based on the bAdd flag set at the beginning of this
             * function: */
            else if (iNumberNewVertices == 1) {

                if (bAddStart) {
                    kVBuffer.SetPosition3( iVertex, m_kStartPoint );
                    kVBuffer.SetNormal3( iVertex, kStartNormal );
                    kVBuffer.SetTCoord3( 0, iVertex, kStartTexCoord );
                    kVBuffer.SetColor4( 0, iVertex, kStartColor );
                } else if (bAddEnd) {
                    kVBuffer.SetPosition3( iVertex, m_kEndPoint );
                    kVBuffer.SetNormal3( iVertex, kEndNormal );
                    kVBuffer.SetTCoord3( 0, iVertex, kEndTexCoord );
                    kVBuffer.SetColor4( 0, iVertex, kEndColor );
                }
            }

            m_akEdgeList[iVertex] = new LinkedList<Integer>();

            /* Calculate straight-line distance to end point*/
            fDistance = distance(m_kEndPoint, kVBuffer.GetPosition3(iVertex) );

            /* Initialize search data variables: */
            m_fRemainingWeight[iVertex] = fPercentage * fDistance;
            m_fWeight[iVertex] = Float.MAX_VALUE;
            m_bRelaxed[iVertex] = false;
            m_iPrevious[iVertex] = -1;
        }


        /* Copy the triangles indices: */
        int iIndexCount = m_kSurface.IBuffer.GetIndexQuantity();
        m_iIndexCount = iIndexCount;

        /* Add 6 * the number of new vertices in triangles: */
        if (bAddStart) {
            m_iIndexCount += 6;
        }

        if (bAddEnd) {
            m_iIndexCount += 6;
        }


        int[] aiIndex = m_kSurface.IBuffer.GetData();
        m_aiIndex = new int[m_iIndexCount];
        
        int iTriCount = 0;
        m_iNumTriangles = iIndexCount / 3;

        int iStart0 = m_aiStartIndex[0];
        int iStart1 = m_aiStartIndex[1];
        int iStart2 = m_aiStartIndex[2];

        int iEnd0 = m_aiEndIndex[0];
        int iEnd1 = m_aiEndIndex[1];
        int iEnd2 = m_aiEndIndex[2];

        int[] aiStartIndex1 = new int[3];
        int[] aiEndIndex1 = new int[3];
        int[] aiStartIndex2 = new int[3];
        int[] aiEndIndex2 = new int[3];

        /* If the start and end vertices fall inside a triangle, do not copy
         * the triangles that the start and end vertices fall in. */
        boolean bStartSkipped = false;
        boolean bEndSkipped = false;

        boolean bSecondStartFound = false;
        boolean bSecondEndFound = false;

        for (int iTri = 0; iTri < m_iNumTriangles; iTri++) {
            int i0 = aiIndex[(iTri * 3) + 0];
            int i1 = aiIndex[(iTri * 3) + 1];
            int i2 = aiIndex[(iTri * 3) + 2];

            boolean bSkip = false;

            /* If the start vertex falls inside a triangle, do not add that
             * triangle to the index list: */
            if (bAddStart && !bStartSkipped) {

                if (triangleEquals(i0, i1, i2, iStart0, iStart1, iStart2)) {
                    bSkip = true;
                    bStartSkipped = true;
                }
            }

            if (bStartOnEdge && !bSecondStartFound) {

                if (!triangleEquals(i0, i1, i2, iStart0, iStart1, iStart2)) {
                    int iIndex = -1;

                    if (iStartEdge == 2) {
                        iIndex = triangleEdge(i0, i1, i2, iStart0, iStart1);

                        if (iIndex != -1) {
                            aiStartIndex1[0] = iStart0;
                            aiStartIndex1[1] = iStart1;
                            aiStartIndex1[2] = iStart2;

                            aiStartIndex2[0] = iStart0;
                            aiStartIndex2[1] = iStart1;
                            aiStartIndex2[2] = iIndex;
                            bSkip = true;
                            bSecondStartFound = true;
                        }
                    } else if (iStartEdge == 1) {
                        iIndex = triangleEdge(i0, i1, i2, iStart0, iStart2);

                        if (iIndex != -1) {
                            aiStartIndex1[0] = iStart2;
                            aiStartIndex1[1] = iStart0;
                            aiStartIndex1[2] = iStart1;

                            aiStartIndex2[0] = iStart2;
                            aiStartIndex2[1] = iStart0;
                            aiStartIndex2[2] = iIndex;
                            bSkip = true;
                            bSecondStartFound = true;
                        }
                    } else if (iStartEdge == 0) {
                        iIndex = triangleEdge(i0, i1, i2, iStart1, iStart2);

                        if (iIndex != -1) {
                            aiStartIndex1[0] = iStart1;
                            aiStartIndex1[1] = iStart2;
                            aiStartIndex1[2] = iStart0;

                            aiStartIndex2[0] = iStart1;
                            aiStartIndex2[1] = iStart2;
                            aiStartIndex2[2] = iIndex;
                            bSkip = true;
                            bSecondStartFound = true;
                        }
                    }
                }
            }

            /* If the end vertex falls inside a triangle, do not add that
             * triangle to the index list: */
            if (bAddEnd && !bEndSkipped) {

                if (triangleEquals(i0, i1, i2, iEnd0, iEnd1, iEnd2)) {
                    bSkip = true;
                    bEndSkipped = true;
                }
            }

            if (bEndOnEdge && !bSecondEndFound) {

                if (!triangleEquals(i0, i1, i2, iEnd0, iEnd1, iEnd2)) {
                    int iIndex = -1;

                    if (iEndEdge == 2) {
                        iIndex = triangleEdge(i0, i1, i2, iEnd0, iEnd1);

                        if (iIndex != -1) {
                            aiEndIndex1[0] = iEnd0;
                            aiEndIndex1[1] = iEnd1;
                            aiEndIndex1[2] = iEnd2;

                            aiEndIndex2[0] = iEnd0;
                            aiEndIndex2[1] = iEnd1;
                            aiEndIndex2[2] = iIndex;
                            bSkip = true;
                            bSecondEndFound = true;
                        }
                    } else if (iEndEdge == 1) {
                        iIndex = triangleEdge(i0, i1, i2, iEnd0, iEnd2);

                        if (iIndex != -1) {
                            aiEndIndex1[0] = iEnd2;
                            aiEndIndex1[1] = iEnd0;
                            aiEndIndex1[2] = iEnd1;

                            aiEndIndex2[0] = iEnd2;
                            aiEndIndex2[1] = iEnd0;
                            aiEndIndex2[2] = iIndex;
                            bSkip = true;
                            bSecondEndFound = true;
                        }
                    } else if (iEndEdge == 0) {
                        iIndex = triangleEdge(i0, i1, i2, iEnd1, iEnd2);

                        if (iIndex != -1) {
                            aiEndIndex1[0] = iEnd1;
                            aiEndIndex1[1] = iEnd2;
                            aiEndIndex1[2] = iEnd0;

                            aiEndIndex2[0] = iEnd1;
                            aiEndIndex2[1] = iEnd2;
                            aiEndIndex2[2] = iIndex;
                            bSkip = true;
                            bSecondEndFound = true;
                        }
                    }
                }
            }

            if (bSkip == false) {
                m_aiIndex[(iTriCount * 3) + 0] = i0;
                m_aiIndex[(iTriCount * 3) + 1] = i1;
                m_aiIndex[(iTriCount * 3) + 2] = i2;

                addEdge(i0, i1);
                addEdge(i0, i2);
                addEdge(i1, i2);

                iTriCount++;
            }
        }

        /* If the start vertex falls inside a triangle, create the three new
         * triangles around the StartPoint using the start indices:
         */
        if (bAddStart && bStartSkipped && !bStartOnEdge) {
            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - iNumberNewVertices;
            m_aiIndex[(iTriCount * 3) + 1] = iStart0;
            m_aiIndex[(iTriCount * 3) + 2] = iStart1;
            iTriCount++;

            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - iNumberNewVertices;
            m_aiIndex[(iTriCount * 3) + 1] = iStart1;
            m_aiIndex[(iTriCount * 3) + 2] = iStart2;
            iTriCount++;

            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - iNumberNewVertices;
            m_aiIndex[(iTriCount * 3) + 1] = iStart2;
            m_aiIndex[(iTriCount * 3) + 2] = iStart0;
            iTriCount++;

            /* Add the edges to the edge list for the start vertex: */
            addEdge(m_iVertexCount - iNumberNewVertices, iStart0);
            addEdge(m_iVertexCount - iNumberNewVertices, iStart1);
            addEdge(m_iVertexCount - iNumberNewVertices, iStart2);
        }
        /* Split two triangles into four new triangles: */
        else if (bAddStart && bSecondStartFound && bStartOnEdge) {
            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - iNumberNewVertices;
            m_aiIndex[(iTriCount * 3) + 1] = aiStartIndex1[2];
            m_aiIndex[(iTriCount * 3) + 2] = aiStartIndex1[0];
            iTriCount++;

            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - iNumberNewVertices;
            m_aiIndex[(iTriCount * 3) + 1] = aiStartIndex1[1];
            m_aiIndex[(iTriCount * 3) + 2] = aiStartIndex1[2];
            iTriCount++;

            addEdge(m_iVertexCount - iNumberNewVertices, aiStartIndex1[0]);
            addEdge(m_iVertexCount - iNumberNewVertices, aiStartIndex1[1]);
            addEdge(m_iVertexCount - iNumberNewVertices, aiStartIndex1[2]);
            addEdge(aiStartIndex1[2], aiStartIndex1[0]);
            addEdge(aiStartIndex1[2], aiStartIndex1[1]);


            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - iNumberNewVertices;
            m_aiIndex[(iTriCount * 3) + 1] = aiStartIndex2[0];
            m_aiIndex[(iTriCount * 3) + 2] = aiStartIndex2[2];
            iTriCount++;

            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - iNumberNewVertices;
            m_aiIndex[(iTriCount * 3) + 1] = aiStartIndex2[2];
            m_aiIndex[(iTriCount * 3) + 2] = aiStartIndex2[1];
            iTriCount++;

            addEdge(m_iVertexCount - iNumberNewVertices, aiStartIndex2[0]);
            addEdge(m_iVertexCount - iNumberNewVertices, aiStartIndex2[1]);
            addEdge(m_iVertexCount - iNumberNewVertices, aiStartIndex2[2]);
            addEdge(aiStartIndex2[2], aiStartIndex2[0]);
            addEdge(aiStartIndex2[2], aiStartIndex2[1]);

        } else {
            bAddStart = false;
        }

        /* If the end vertex falls inside a triangle, create the three new
         * triangles around the EndPoint using the start indices:
         */
        if (bAddEnd && bEndSkipped && !bEndOnEdge) {
            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - 1;
            m_aiIndex[(iTriCount * 3) + 1] = iEnd0;
            m_aiIndex[(iTriCount * 3) + 2] = iEnd1;
            iTriCount++;

            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - 1;
            m_aiIndex[(iTriCount * 3) + 1] = iEnd1;
            m_aiIndex[(iTriCount * 3) + 2] = iEnd2;
            iTriCount++;

            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - 1;
            m_aiIndex[(iTriCount * 3) + 1] = iEnd2;
            m_aiIndex[(iTriCount * 3) + 2] = iEnd0;
            iTriCount++;

            /* Add the edges to the edge list for the end vertex: */
            addEdge(m_iVertexCount - 1, iEnd0);
            addEdge(m_iVertexCount - 1, iEnd1);
            addEdge(m_iVertexCount - 1, iEnd2);
        }
        /* Split two triangles into four new triangles: */
        else if (bAddEnd && bSecondEndFound && bEndOnEdge) {
            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - 1;
            m_aiIndex[(iTriCount * 3) + 1] = aiEndIndex1[2];
            m_aiIndex[(iTriCount * 3) + 2] = aiEndIndex1[0];
            iTriCount++;

            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - 1;
            m_aiIndex[(iTriCount * 3) + 1] = aiEndIndex1[1];
            m_aiIndex[(iTriCount * 3) + 2] = aiEndIndex1[2];
            iTriCount++;

            addEdge(m_iVertexCount - 1, aiEndIndex1[0]);
            addEdge(m_iVertexCount - 1, aiEndIndex1[1]);
            addEdge(m_iVertexCount - 1, aiEndIndex1[2]);
            addEdge(aiEndIndex1[2], aiEndIndex1[0]);
            addEdge(aiEndIndex1[2], aiEndIndex1[1]);


            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - 1;
            m_aiIndex[(iTriCount * 3) + 1] = aiEndIndex2[0];
            m_aiIndex[(iTriCount * 3) + 2] = aiEndIndex2[2];
            iTriCount++;

            m_aiIndex[(iTriCount * 3) + 0] = m_iVertexCount - 1;
            m_aiIndex[(iTriCount * 3) + 1] = aiEndIndex2[2];
            m_aiIndex[(iTriCount * 3) + 2] = aiEndIndex2[1];
            iTriCount++;

            addEdge(m_iVertexCount - 1, aiEndIndex2[0]);
            addEdge(m_iVertexCount - 1, aiEndIndex2[1]);
            addEdge(m_iVertexCount - 1, aiEndIndex2[2]);
            addEdge(aiEndIndex2[2], aiEndIndex2[0]);
            addEdge(aiEndIndex2[2], aiEndIndex2[1]);

        } else {
            bAddEnd = false;
        }

        m_iNumTriangles += 2 * iNumberNewVertices;

        /* Store the new Mesh in m_kModified: */
        IndexBuffer kIBuffer = new IndexBuffer( m_aiIndex.length, m_aiIndex );
        m_kModified = new TriMesh( kVBuffer, kIBuffer);
        m_kModified.SetName( m_kSurface.GetName() );
        aiIndex = null;

        if (bFirstSegment) {
            m_kFirstPoint = new Vector3f(m_kEndPoint);
            m_aiFirstIndex[0] = m_aiEndIndex[0];
            m_aiFirstIndex[1] = m_aiEndIndex[1];
            m_aiFirstIndex[2] = m_aiEndIndex[2];
        }
    }


    /**
     * onRight determines if a given point on a triangle, kSide, is to the right of or to the left of the vector
     * specified by the vertices kPrev - kPoint.
     *
     * @param   kMesh       TriMesh surface mesh
     * @param   iPrevIndex  int previous vertice
     * @param   iIndex      int specified vertice index
     * @param   iSideIndex  int side vertice index
     *
     * @return  boolean a point on the right or triangle or not.
     */
    private boolean onRight(TriMesh kMesh, int iPrevIndex, int iIndex, int iSideIndex) {
        Vector3f kPrev = new Vector3f();
        Vector3f kPoint = new Vector3f();
        Vector3f kSide = new Vector3f();

        kMesh.VBuffer.GetPosition3(iPrevIndex, kPrev);
        kMesh.VBuffer.GetPosition3(iIndex, kPoint);
        kMesh.VBuffer.GetPosition3(iSideIndex, kSide);

        Vector3f kMeshNormal = new Vector3f();
        kMesh.VBuffer.GetNormal3(iIndex, kMeshNormal);

        Vector3f kSide_Point = Vector3f.sub( kSide, kPoint );
        kSide_Point.normalize();

        Vector3f kPrev_Point = Vector3f.sub( kPrev, kPoint );
        kPrev_Point.normalize();

        Vector3f kCross = Vector3f.cross( kPrev_Point, kSide_Point );
        boolean bReturn = true;

        if (kCross.dot(kMeshNormal) < 0) {
            bReturn = false;
        }

        kPrev = null;
        kPoint = null;
        kSide = null;
        kMeshNormal = null;
        kSide_Point = null;
        kPrev_Point = null;
        kCross = null;

        return bReturn;
    }

    /**
     * creating new meshes to export to the scene graph.
     */
    private void outputDeletedAsNew(TriMesh kSourceMesh) {
        LinkedList<Vector3f> kVertices = new LinkedList<Vector3f>();
        LinkedList<Vector3f> kNormals = new LinkedList<Vector3f>();
        LinkedList<Vector3f> kTexCoords = new LinkedList<Vector3f>();
        LinkedList<ColorRGBA> kColors = new LinkedList<ColorRGBA>();

        int iNumDeletedTris = 0;
        int iNumTris = m_iIndexCount / 3;

        for (int iTri = 0; iTri < iNumTris; iTri++) {

            if (m_abRemoveTris[iTri] == true) {
                iNumDeletedTris++;
            }
        }

        int[] aiConnect = new int[iNumDeletedTris * 3];
        int iNumNewTris = 0;

        for (int iTri = 0; iTri < iNumTris; iTri++) {

            if (m_abRemoveTris[iTri] == true) {
                int i0 = m_aiIndex[(iTri * 3) + 0];
                int i1 = m_aiIndex[(iTri * 3) + 1];
                int i2 = m_aiIndex[(iTri * 3) + 2];

                /* Add to the vertex list: */
                Vector3f kPos0 = kSourceMesh.VBuffer.GetPosition3(i0);
                Vector3f kPos1 = kSourceMesh.VBuffer.GetPosition3(i1);
                Vector3f kPos2 = kSourceMesh.VBuffer.GetPosition3(i2);
                int iNewIndex0 = containsVector( kVertices, kPos0 );
                int iNewIndex1 = containsVector( kVertices, kPos1 );
                int iNewIndex2 = containsVector( kVertices, kPos2 );
                if ( iNewIndex0 == -1 ) 
                {
                    iNewIndex0 = kVertices.size();
                    kVertices.add(kPos0);
                    kNormals.add(kSourceMesh.VBuffer.GetNormal3(i0));
                    kTexCoords.add(kSourceMesh.VBuffer.GetTCoord3(0, i0));
                    kColors.add(kSourceMesh.VBuffer.GetColor4(0, i0));
                }
                if ( iNewIndex1 == -1 ) 
                {
                    iNewIndex1 = kVertices.size();
                    kVertices.add(kPos1);
                    kNormals.add(kSourceMesh.VBuffer.GetNormal3(i1));
                    kTexCoords.add(kSourceMesh.VBuffer.GetTCoord3(0, i1));
                    kColors.add(kSourceMesh.VBuffer.GetColor4(0, i1));
                }
                if ( iNewIndex2 == -1 ) 
                {
                    iNewIndex2 = kVertices.size();
                    kVertices.add(kPos2);
                    kNormals.add(kSourceMesh.VBuffer.GetNormal3(i2));
                    kTexCoords.add(kSourceMesh.VBuffer.GetTCoord3(0, i2));
                    kColors.add(kSourceMesh.VBuffer.GetColor4(0, i2));
                }

                /* Add new tri to the index list: */
                aiConnect[(iNumNewTris * 3) + 0] = iNewIndex0;
                aiConnect[(iNumNewTris * 3) + 1] = iNewIndex1;
                aiConnect[(iNumNewTris * 3) + 2] = iNewIndex2;
                iNumNewTris++;
            }
        }

        VertexBuffer kVBuffer = new VertexBuffer( kSourceMesh.VBuffer.GetAttributes(), kVertices.size() );
        for (int i = 0; i < kVertices.size(); i++) {
            kVBuffer.SetPosition3(i, kVertices.get(i));
            kVBuffer.SetNormal3(i, kNormals.get(i));
            kVBuffer.SetTCoord3(0, i, kTexCoords.get(i));
            kVBuffer.SetColor4(0, i, kColors.get(i));
        }

        kVertices.clear();
        kVertices = null;
        kNormals.clear();
        kNormals = null;
        kTexCoords.clear();
        kTexCoords = null;
        kColors.clear();
        kColors = null;

        TriMesh kAddMesh = new TriMesh( kVBuffer, new IndexBuffer( aiConnect.length, aiConnect ) );
        kAddMesh.SetName( "Geodesic_" + m_iNumNewMeshes++ + ".sur" );
         if (m_kPanel != null) {
             m_kPanel.addSurface(kAddMesh);
         }
    }

    /**
     * This function determines the shortest path from the start vertex through the input vertex to the vertices that
     * neighbor the input vertex.
     *
     * <p>The function relaxEdges looks at all the vertices connected by triangle edges to the input vertex and
     * calculated the weight factors for each of those vertices, adding those vertices that are not yet "relaxed" to the
     * list of vertices on the border.</p>
     *
     * <p>Once the weight factors for each of the vertices connected to the input vertex are set, the input vertex is
     * labeled "relaxed" and removed from the list of vertices on the border.</p>
     *
     * @param  iNode  int input vertex index
     */
    private void relaxEdges(TriMesh kMesh, int iNode) {
        int iNeighbor;
        Integer kNeighbor;
        int iPreviousSave;
        float fPathLength;

        /* Get the number of edges that are connected to the input vertex: */
        int iNumEdges = m_akEdgeList[iNode].size();

        /* Loop over all edges: */
        for (int iEdge = 0; iEdge < iNumEdges; iEdge++) {

            /* iNeighbor is the index of the vertex connected by the current
             * edge: */
            iNeighbor = m_akEdgeList[iNode].get(iEdge).intValue();

            if (m_bRelaxed[iNeighbor]) {
                continue;
            }

            /* Calculated the edge distance between the neighbor vertex and
             * the input vertex. This is done by looking at the smoothed path through the current node to the neighbor.
             * If the neighbor already has a path through it, then save it incase it's
             * smaller: */
            iPreviousSave = m_iPrevious[iNeighbor];

            if (iNeighbor != iNode) {
                m_iPrevious[iNeighbor] = iNode;
            } else {
                m_iPrevious[iNeighbor] = iNode;
            }

            fPathLength = m_fWeight[iNode] + distance(kMesh.VBuffer.GetPosition3(iNode),
                    kMesh.VBuffer.GetPosition3(iNeighbor));


            /* If this is the first time iNeighbor has been touched, or if the
             * path is shorter passing through the input node, then update the
             * wieght: */
            if ((iPreviousSave == -1) || (m_fWeight[iNeighbor] > fPathLength)) {
                m_fWeight[iNeighbor] = fPathLength;

                /* If the neighbor vertex has not yet been relaxed, add it to
                 * the list of vertices on the border -- so that it can be
                 * relaxed */
                if (!m_bRelaxed[iNeighbor]) {
                    kNeighbor = new Integer(iNeighbor);

                    if (!m_kBorder.contains(kNeighbor)) {
                        m_kBorder.add(kNeighbor);
                    } else {
                        kNeighbor = null;
                    }
                }
            }
            /* Otherwise, revert to the original path for iNeighbor: */
            else {

                if (iNeighbor != iPreviousSave) {
                    m_iPrevious[iNeighbor] = iPreviousSave;
                } else {
                    m_iPrevious[iNeighbor] = iPreviousSave;
                }
            }
        }

        /* Set the relaxed flag for this vertex to true. We now know the
         * shortest path from the start vertex to this vertex. */
        m_bRelaxed[iNode] = true;

        /* Remove this vertex from the list of vertices that can be
         * relaxed. */
        Integer kINode = new Integer(iNode);

        if (m_kBorder.contains(kINode)) {
            m_kBorder.remove(kINode);
        }

        kINode = null;
    }

    /**
     * Remove all the geodesic lines and reset to draw the new geodesic lines.
     */
    private void resetDrawGeodesic() {

        /* Delete CutList variables: */
        m_kGeodesic_Working.clear();
        m_kGeodesic_Working_Left.clear();
        m_kGeodesic_Working_Right.clear();
        m_kGeodesic_Finished.clear();
        m_iNumWorking = 0;

        /* Clear line drawing: */
        m_bEnabled = false;
        m_kPanel.removeAllGeodesic( m_kSurface );
        m_iSmoothedGeodesicGroup = 0;
        m_iDijkstraGeodesicGroup = 0;
        m_iEuclidianGeodesicGroup = 0;

        m_bLastWire = false;
        clearAllStartEnd();

        m_bGroupAdded = false;

        m_iNumGeodesicVertices = 0;
        m_kGeodesicVertices = null;

        m_iNumPicked = 0;
        m_kFirstPoint = null;
        m_aiFirstIndex = null;
    }

    /**
     * In livewire mode the endpoints of each path segment along Dijkstra's curve -- between each of the user-selected
     * points -- are stored so that the smoothed geodesic may be calculated and displayed later.
     */
    private void saveStartEnd() {
        Vector3f kEndIndex = new Vector3f(m_aiEndIndex[0], m_aiEndIndex[1], m_aiEndIndex[2]);

        Vector3f kStartIndex = new Vector3f(m_aiStartIndex[0], m_aiStartIndex[1], m_aiStartIndex[2]);
        LinkedList<Vector3f> kStartEndList = new LinkedList<Vector3f>();
        kStartEndList.add(0, new Vector3f(kEndIndex));
        kStartEndList.add(1, new Vector3f(kStartIndex));
        kStartEndList.add(2, new Vector3f(m_kEndPoint));
        kStartEndList.add(3, new Vector3f(m_kStartPoint));

        if (m_kStartEndList == null) {
            m_kStartEndList = new LinkedList<LinkedList<Vector3f>>();
        }

        m_kStartEndList.add(kStartEndList);
    }

    /**
     * The start and end surfaces ensure that the points on the geodesic curve all fall on one mesh, and that the
     * algorithm isn't trying to find a path between two unconnected meshes.
     *
     * @param  kMesh  TriMesh surface mesh
     */
    private void setEndSurface(TriMesh kMesh) {
        m_kEndSurface = kMesh;

         if (m_kEndSurface == m_kStartSurface) {
             setSurface(kMesh);
         }
    }

    /**
     * Sets the previous start point of the geodesic curve on the mesh. Used when the last point is deleted, the start
     * point reverts to the previous start point. The point coordinates must be in local mesh coordinates.
     *
     * @param  kPoint  the point on the triangle mesh where the geodesic curve is to start, in mesh coordinates.
     */
    private void setPreviousStartPoint(Vector3f kPoint) {

        if (m_kPreviousStartPoint != null) {
            m_kPreviousStartPoint = null;
        }

        m_kPreviousStartPoint = new Vector3f(kPoint);
    }

    /**
     * The start and end surfaces ensure that the points on the geodesic curve all fall on one mesh, and that the
     * algorithm isn't trying to find a path between two unconnected meshes.
     *
     * @param  kMesh  surface mesh
     */
    private void setStartSurface(TriMesh kMesh) {
        m_kStartSurface = kMesh;
    }

    /**
     * Access function to set the triangle mesh that the geodesic curve is calculated on.
     *
     * @param  kMesh  the new triangle mesh
     */
    private void setSurface(TriMesh kMesh) {
        m_kSurface = kMesh;

        if (m_kOriginal == null) {
            m_kOriginal = new TriMesh(kMesh);
            m_kOriginal.SetName( kMesh.GetName() );
            m_kLastFinished = m_kOriginal;
            m_kFinished = m_kOriginal;
            m_kLastCut = m_kOriginal;
        }
    }


    /**
     * Smooth the path.
     *
     * @param   iNode         int vertex index
     * @param   kLeft         LinkedList left point
     * @param   kMiddle       LinkedList middle point
     * @param   kRight        LinkedList right point
     * @param   kLeftTemp     LinkedList left point temporary
     * @param   kRightTemp    LinkedList right point temporary
     * @param   kNewVertTemp  LinkedList new point temporary
     *
     * @return  int success or not
     */
    private int smoothPath( TriMesh kMesh, int iNode, LinkedList<Integer> kLeft, LinkedList<Integer> kMiddle, LinkedList<Integer> kRight, LinkedList<Integer> kLeftTemp,
                           LinkedList<Integer> kRightTemp, LinkedList<Vector4f> kNewVertTemp) {
        Vector3f kStart = kMesh.VBuffer.GetPosition3(kMiddle.get(iNode - 1).intValue());

        int iMiddle = kMiddle.get(iNode).intValue();
        int iEnd = iNode + 1;

        for (iEnd = iNode + 1; iEnd < kMiddle.size(); iEnd++) {

            if (kMiddle.get(iEnd).intValue() != iMiddle) {
                break;
            }
        }

        if (iEnd == kMiddle.size()) {
            System.err.println("error smoothPath");
            System.exit(-1);
        }

        int iNumSteps = iEnd - iNode;

        Vector3f kEnd = kMesh.VBuffer.GetPosition3(kMiddle.get(iEnd).intValue());
        Vector4f kNewPoint = new Vector4f();

        boolean bUseRight = true;
        float fRightPathLength = 0;
        int iRight = kRight.get(iNode).intValue();

        if (iRight != -1) {
            fRightPathLength += findMin(kMesh, kStart, iMiddle, iRight, kEnd, kNewPoint);

            for (int iRightNode = iNode + 1; iRightNode < (iNode + iNumSteps); iRightNode++) {

                if (iRight == kRight.get(iRightNode).intValue()) {
                    break;
                }

                iRight = kRight.get(iRightNode).intValue();

                if (iRight != -1) {
                    fRightPathLength += findMin(kMesh, kStart, iMiddle, iRight, kEnd, kNewPoint);
                } else {
                    fRightPathLength = 0;
                    bUseRight = false;

                    break;
                }

                iRightNode++;
            }
        } else {
            bUseRight = false;
        }

        boolean bUseLeft = true;
        float fLeftPathLength = 0;
        int iLeft = kLeft.get(iNode).intValue();

        if (iLeft != -1) {
            fLeftPathLength += findMin(kMesh, kStart, iMiddle, iLeft, kEnd, kNewPoint);

            for (int iLeftNode = iNode + 1; iLeftNode < (iNode + iNumSteps); iLeftNode++) {

                if (iLeft == kLeft.get(iLeftNode).intValue()) {
                    break;
                }

                iLeft = kLeft.get(iLeftNode).intValue();

                if (iLeft != -1) {
                    fLeftPathLength += findMin(kMesh, kStart, iMiddle, iLeft, kEnd, kNewPoint);
                } else {
                    bUseLeft = false;
                    fLeftPathLength = 0;

                    break;
                }

                iLeftNode++;
            }
        } else {
            bUseLeft = false;
        }

        Vector3f kMiddlePoint = kMesh.VBuffer.GetPosition3(iMiddle);

        if ((bUseRight && bUseLeft && (fRightPathLength <= fLeftPathLength)) || (bUseRight && !bUseLeft)) {
            iRight = kRight.get(iNode).intValue();

            findMin(kMesh, kStart, iMiddle, iRight, kEnd, kNewPoint);
            kLeftTemp.add(new Integer(iMiddle));
            kRightTemp.add(new Integer(iRight));
            kNewVertTemp.add(new Vector4f(kNewPoint));

            for (int iRightNode = iNode + 1; iRightNode < (iNode + iNumSteps); iRightNode++) {

                if (iRight == kRight.get(iRightNode).intValue()) {
                    break;
                }

                iRight = kRight.get(iRightNode).intValue();

                findMin(kMesh, kStart, iMiddle, iRight, kEnd, kNewPoint);
                kLeftTemp.add(new Integer(iMiddle));
                kRightTemp.add(new Integer(iRight));
                kNewVertTemp.add(new Vector4f(kNewPoint));
            }

            return 0;
        } else if ((bUseLeft && bUseRight && (fLeftPathLength < fRightPathLength)) || (bUseLeft && !bUseRight)) {
            iLeft = kLeft.get(iNode).intValue();

            findMin(kMesh, kStart, iMiddle, iLeft, kEnd, kNewPoint);
            kLeftTemp.add(new Integer(iLeft));
            kRightTemp.add(new Integer(iMiddle));
            kNewVertTemp.add(new Vector4f(kNewPoint));

            for (int iLeftNode = iNode + 1; iLeftNode < (iNode + iNumSteps); iLeftNode++) {

                if (iLeft == kLeft.get(iLeftNode).intValue()) {
                    break;
                }

                iLeft = kLeft.get(iLeftNode).intValue();

                findMin(kMesh, kStart, iMiddle, iLeft, kEnd, kNewPoint);
                kLeftTemp.add(new Integer(iLeft));
                kRightTemp.add(new Integer(iMiddle));
                kNewVertTemp.add(new Vector4f(kNewPoint));
            }

            return 1;
        }

        kLeftTemp.add(new Integer(-1));
        kRightTemp.add(new Integer(-1));
        kNewVertTemp.add(new Vector4f(kMiddlePoint.X, kMiddlePoint.Y, kMiddlePoint.Z, iMiddle));

        return -1;
    }

    /**
     * Used when new triangles are added to the mesh, either when the mesh is triangulated along the smoothed geodesic
     * curve, or when the mesh is cut. sortTriIndex sorts the triangle indices so that the triangle is always
     * front-facing and that the normals are correct for rendering
     *
     * @param  aiAddTri    Point3i added triangle vertices
     */
    private void sortTriIndex(Vector3f aiAddTri, VertexBuffer kVBuffer) {
        int i0 = (int)aiAddTri.X;
        int i1 = (int)aiAddTri.Y;
        int i2 = (int)aiAddTri.Z;

        Vector3f kP0 = kVBuffer.GetPosition3( i0 );
        Vector3f kP1 = kVBuffer.GetPosition3( i1 );
        Vector3f kP2 = kVBuffer.GetPosition3( i2 );

        Vector3f kP1_P0 = Vector3f.sub( kP1, kP0 );
        kP1_P0.normalize();

        Vector3f kP2_P0 = Vector3f.sub( kP2, kP0 );
        kP2_P0.normalize();

        Vector3f kCross = Vector3f.cross( kP1_P0, kP2_P0 );

        Vector3f kNormal = kVBuffer.GetNormal3( i0 );
        if (kCross.dot(kNormal) < 0)
        {
            aiAddTri.set( i1, i0, i2 );
        }

        kP0 = null;
        kP1 = null;
        kP2 = null;
        kP1_P0 = null;
        kP2_P0 = null;
        kCross = null;
        kNormal = null;
    }

    /**
     * if the two of the first three triangle indices equal the second two indices, then the third index is returned.
     *
     * @param   i0   int first triangle indice 1
     * @param   i1   int first triangle indice 2
     * @param   i2   int first triangle indice 3
     * @param   iP0  int second triangle indice 1
     * @param   iP1  int second triangle indice 2
     *
     * @return  int third index
     */
    private int triangleEdge(int i0, int i1, int i2, int iP0, int iP1) {

        if ((i0 == iP0) && (i1 == iP1)) {
            return i2;
        }

        if ((i0 == iP1) && (i1 == iP0)) {
            return i2;
        }

        if ((i2 == iP0) && (i1 == iP1)) {
            return i0;
        }

        if ((i2 == iP1) && (i1 == iP0)) {
            return i0;
        }

        if ((i2 == iP0) && (i0 == iP1)) {
            return i1;
        }

        if ((i2 == iP1) && (i0 == iP0)) {
            return i1;
        }

        return -1;
    }

    /**
     * Returns true if the first three triangle indices equal the second three indices.
     *
     * @param   i0    first triangle indice 0
     * @param   i1    first triangle indice 1
     * @param   i2    first triangle indice 2
     * @param   iP0   second triangle indice 0
     * @param   iP1   second triangle indice 1
     * @param   iP2   second triangle indice 2
     *
     * @return  boolean true equal, false not
     */
    private boolean triangleEquals(int i0, int i1, int i2, int iP0, int iP1, int iP2) {

        if ((i0 == iP0) && (i1 == iP1) && (i2 == iP2)) {
            return true;
        }

        if ((i0 == iP0) && (i1 == iP2) && (i2 == iP1)) {
            return true;
        }

        if ((i0 == iP1) && (i1 == iP0) && (i2 == iP2)) {
            return true;
        }

        if ((i0 == iP1) && (i1 == iP2) && (i2 == iP0)) {
            return true;
        }

        if ((i0 == iP2) && (i1 == iP0) && (i2 == iP1)) {
            return true;
        }

        if ((i0 == iP2) && (i1 == iP1) && (i2 == iP0)) {
            return true;
        }

        return false;
    }

    /**
     * Check to see if the triangle specified in the triangle list.
     *
     * @param   kTri  Point3i triangle specified
     *
     * @return  boolean true in the list, false not in the list
     */
    private boolean triangleExists(Vector3f kTri) {
        int iNumTris = m_iIndexCount / 3;

        for (int iTri = 0; iTri < iNumTris; iTri++) {
            int i0 = m_aiIndex[(iTri * 3) + 0];
            int i1 = m_aiIndex[(iTri * 3) + 1];
            int i2 = m_aiIndex[(iTri * 3) + 2];

            if (triangleEquals(i0, i1, i2, (int)kTri.X, (int)kTri.Y, (int)kTri.Z)) {
                return true;
            }
        }

        return false;
    }

    /**
     * triangulates the mesh along a single geodesic.
     *
     * @param   kMesh         TriMesh surface mesh
     * @param   kPath         LinkedList path link list
     * @param   kLeftPath     LinkedList left path link list
     * @param   kRightPath    LinkedList right path link list
     * @param   iVertexCount  int vertex count
     *
     * @return  int number of vertices
     */
    private int triangulate(TriMesh kMesh,
                            LinkedList kPath, LinkedList kLeftPath, LinkedList kRightPath,
                            int iVertexCount)
    {
        int iNumNewVerts = 0;

        if (kPath.size() < 2) {
            return iNumNewVerts;
        }

        /* The first point on the Geodesic Path is already in the mesh, by
         * definition: */
        Vector4f kPathPoint = (Vector4f)kPath.get(0);
        Vector4f kNextPoint;

        /* Identify which point on the mesh this is: */
        int iPathIndex = (int) kPathPoint.W;
        int iNextPathIndex;

        if (iPathIndex == -1) {
            return iNumNewVerts;
        }

        /* Triangle index to add and delete: */
        Vector3f kAddTri = new Vector3f();
        Vector3f kDeleteTri = new Vector3f();

        int[] iSideIndex = new int[2];
        int iPath = 1;

        for (iPath = 1; iPath < kPath.size(); iPath++) {
            kNextPoint = (Vector4f) kPath.get(iPath);
            iNextPathIndex = (int) kNextPoint.W;

            if (iPathIndex == iNextPathIndex) {
                continue;
            }

            if (iNextPathIndex != -1) {

                if (iPathIndex < iVertexCount) {

                    iPathIndex = iNextPathIndex;
                } else {

                    /* The current vertex is not on the mesh, the next vertex
                     * is: */
                    iSideIndex[0] = ((Integer) kLeftPath.get(iPath - 1)).intValue();
                    iSideIndex[1] = ((Integer) kRightPath.get(iPath - 1)).intValue();

                    /* Remove the triangle containing the side indexes and the
                     * next vertex: */
                    kDeleteTri.set( iSideIndex[0], iSideIndex[1], iNextPathIndex );

                    if (triangleExists(kDeleteTri)) {
                        if (!contains(m_kRemoveTriangles, kDeleteTri)) {
                            m_kRemoveTriangles.add(new Vector3f(kDeleteTri));
                        }

                        /* Add two new triangles and quit: */
                        kAddTri.set( iSideIndex[0], iPathIndex, iNextPathIndex );
                        m_kNewTriangles.add(new Vector3f(kAddTri));

                        kAddTri.set( iSideIndex[1], iPathIndex, iNextPathIndex );
                        m_kNewTriangles.add(new Vector3f(kAddTri));
                    }

                    iPathIndex = iNextPathIndex;
                }
            } else {
                iNextPathIndex = iVertexCount + iNumNewVerts;

                /* Update the path point from a -1 (unknown) index to the new
                 * index value: */
                ((Vector4f) kPath.get(iPath)).W = iNextPathIndex;
                iSideIndex[0] = ((Integer) kLeftPath.get(iPath)).intValue();
                iSideIndex[1] = ((Integer) kRightPath.get(iPath)).intValue();

                /* the first vertex is on the mesh, the next vertex is not on
                 * the mesh: */
                if (iPathIndex < iVertexCount) {

                    /* remove the triangle containing the first
                     * vertex and the two new sides: */
                    kDeleteTri.set( iPathIndex, iSideIndex[0], iSideIndex[1] );

                    if (triangleExists(kDeleteTri)) {

                        if (!contains(m_kRemoveTriangles, kDeleteTri)) {
                            m_kRemoveTriangles.add(new Vector3f(kDeleteTri));
                        }

                        /* Add two new triangles: */
                        kAddTri.set( iPathIndex, iSideIndex[0], iNextPathIndex );
                        m_kNewTriangles.add(new Vector3f(kAddTri));

                        kAddTri.set( iPathIndex, iSideIndex[1], iNextPathIndex );
                        m_kNewTriangles.add(new Vector3f(kAddTri));

                        /* Add the next vertex: */
                        m_kNewVerts.add(new Vector3f(kNextPoint.X, kNextPoint.Y, kNextPoint.Z));

                        /* Add the new normal: */
                        m_kNewNormals.add(getNormal(kMesh, iSideIndex[0], iSideIndex[1]));

                        /* Add the new texCoord: */
                        m_kNewTexCoords.add(new Vector3f(getTexCoord(kMesh, iSideIndex[0], iSideIndex[1])));

                        /* Add the new color: */
                        m_kNewColors.add(new ColorRGBA(getColor(kMesh, iSideIndex[0], iSideIndex[1])));

                        iNumNewVerts++;
                    }

                    iPathIndex = iNextPathIndex;
                } else {

                    /* neither vertex is on the mesh: */
                    int iPrevSide0 = ((Integer) kLeftPath.get(iPath - 1)).intValue();
                    int iPrevSide1 = ((Integer) kRightPath.get(iPath - 1)).intValue();
                    int iPrevSide = iPrevSide0;

                    if ((iPrevSide0 == iSideIndex[1]) || (iPrevSide1 == iSideIndex[1])) {

                        /* Swap side indexes: */
                        int iTemp = iSideIndex[0];
                        iSideIndex[0] = iSideIndex[1];
                        iSideIndex[1] = iTemp;
                    }

                    if (iPrevSide == iSideIndex[0]) {
                        iPrevSide = iPrevSide1;
                    }


                    /* Delete the triangle: */
                    kDeleteTri.set( iPrevSide, iSideIndex[0], iSideIndex[1] );

                    if (triangleExists(kDeleteTri)) {
                        if (!contains(m_kRemoveTriangles, kDeleteTri)) {
                            m_kRemoveTriangles.add(new Vector3f(kDeleteTri));
                        }

                        /* Add three new triangles: */
                        kAddTri.set( iPathIndex, iNextPathIndex, iSideIndex[0] );
                        m_kNewTriangles.add(new Vector3f(kAddTri));

                        kAddTri.set( iPathIndex, iNextPathIndex, iPrevSide );
                        m_kNewTriangles.add(new Vector3f(kAddTri));

                        kAddTri.set( iPrevSide, iNextPathIndex, iSideIndex[1] );
                        m_kNewTriangles.add(new Vector3f(kAddTri));

                        /* Add the next vertex: */
                        m_kNewVerts.add(new Vector3f(kNextPoint.X, kNextPoint.Y, kNextPoint.Z));

                        /* Add the new normal: */
                        m_kNewNormals.add(new Vector3f(getNormal(kMesh, iSideIndex[0], iSideIndex[1])));

                        /* Add the new texCoord: */
                        m_kNewTexCoords.add(new Vector3f(getTexCoord(kMesh, iSideIndex[0], iSideIndex[1])));

                        /* Add the new color: */
                        m_kNewColors.add(new ColorRGBA(getColor(kMesh, iSideIndex[0], iSideIndex[1])));

                        iNumNewVerts++;
                    }

                    iPathIndex = iNextPathIndex;

                }
            }
        }

        return iNumNewVerts;
    }


    /**
     * re-triangulates the mesh along the Smoothed Geodesic.
     */
    private void triangulateMeshPath() {

//         //m_kModified = m_kSurface;
//         m_aiIndex = m_kModified.IBuffer.GetData();
        m_kNewVerts = new LinkedList<Vector3f>();
        m_kNewNormals = new LinkedList<Vector3f>();
        m_kNewTexCoords = new LinkedList<Vector3f>();
        m_kNewColors = new LinkedList<ColorRGBA>();
        m_kNewTriangles = new LinkedList<Vector3f>();
        m_kRemoveTriangles = new LinkedList<Vector3f>();

        /* Get the vertex counts: */
        int iVertexCount = m_kModified.VBuffer.GetVertexQuantity();
        int iOldVertexCount = m_kModified.VBuffer.GetVertexQuantity();

        /* First, triangulate the last Geodesic_Working path: */
        LinkedList kOpenPath = m_kGeodesic_Working.get(m_iNumWorking - 1);
        LinkedList kOpenPathLeft = m_kGeodesic_Working_Left.get(m_iNumWorking - 1);
        LinkedList kOpenPathRight = m_kGeodesic_Working_Right.get(m_iNumWorking - 1);
        
        //System.err.println("Triangulate");
        iVertexCount += triangulate(m_kModified, kOpenPath, kOpenPathLeft, kOpenPathRight, iVertexCount);
        //System.err.println("Triangulate done");
        
//         System.err.println( "triangulateMeshPath calling creatNewMesh " + iVertexCount + " " + iOldVertexCount );
        TriMesh kCutMesh = createNewMesh(m_kModified, iVertexCount, iOldVertexCount);
        kCutMesh.SetName( m_kModified.GetName() );

//         System.err.println("CreateNewMesh done");
        /* Delete m_kModified, and set to kCutMesh: */
        m_kModified = null;
        m_kModified = kCutMesh;

        /* Export the modified original and the new meshes to the
         * surRenderer:
         */
        if (m_kPanel != null) {

            // Backup the unmodified mesh:
            m_kSurfaceBackup = null;
            m_kSurfaceBackup = new TriMesh(m_kSurface);
            m_kSurfaceBackup.SetName( m_kSurface.GetName() );
            m_kPanel.replaceMesh(m_kSurface, m_kModified);
            m_kSurface = null;
            m_kSurface = m_kModified;
            m_kStartSurface = m_kModified;
        }

        /* Delete local vars: */
        m_kNewVerts.clear();
        m_kNewVerts = null;
        m_kNewNormals.clear();
        m_kNewNormals = null;
        m_kNewTexCoords.clear();
        m_kNewTexCoords = null;
        m_kNewColors.clear();
        m_kNewColors = null;
        m_kNewTriangles.clear();
        m_kNewTriangles = null;
        m_kRemoveTriangles.clear();
        m_kRemoveTriangles = null;

    }

    /**
     * unZip cuts along the smoothed geodesic path by creating new vertices for each point on the path, and updating the
     * triangles that are connected to the path and that fall on the right of the path to contain the new path vertices.
     * This cuts the mesh by disconnecting the triangles that are on the left and right sides of the geodesic path.
     *
     * @param   kMesh         TriMesh surface mesh
     * @param   kPath         LinkedList path link list
     * @param   iVertexCount  int vertex count
     * @param   bOpen         boolean closed path or not
     * @param   kNewPath      LinkedList new path link list
     *
     * @return  int new number of vertices
     */
    private int unZip(TriMesh kMesh, LinkedList<Vector4f> kPath, int iVertexCount, boolean bOpen, LinkedList<Integer> kNewPath) {
        Vector3f kAddTri = new Vector3f();
        Vector3f kDeleteTri = new Vector3f();

        Vector3f kNewNormal = new Vector3f();
        Vector3f kNewTexCoord = new Vector3f();
        ColorRGBA kNewColor = new ColorRGBA();
        Vector4f kNewVertex = new Vector4f();

        int iPathIndex;
        int iPrevPathIndex;
        int iNextPathIndex;

        LinkedList<Integer> kEndList = new LinkedList<Integer>();
        LinkedList<Integer> kEndPoints = new LinkedList<Integer>();
        int iNumEndPoints;
        int iEndIndex = -1;
        int iPath;

        iPath = 1;

        while (iPath < kPath.size()) {

            if ((kPath.get(iPath - 1)).W == (kPath.get(iPath)).W) {
                kPath.remove(iPath);
            } else {
                iPath++;
            }
        }

        /* Add the first path index to the new list: */
        if (!bOpen && (kNewPath != null)) {
            kNewVertex = kPath.get(0);
            iPathIndex = (int) kNewVertex.W;
            kNewPath.add(new Integer(getPathIndex(kPath, iVertexCount, iPathIndex, bOpen)));
        }

        // for ( iPath = 1; iPath < (kPath.size() -1); iPath++ )
        for (iPath = 1; iPath < kPath.size(); iPath++) {

            /* Get the path point, index, and normal to duplicate: */
            kNewVertex = kPath.get(iPath);
            iPathIndex = (int) kNewVertex.W;
            kMesh.VBuffer.GetNormal3(iPathIndex, kNewNormal);
            kMesh.VBuffer.GetTCoord3(0, iPathIndex, kNewTexCoord);
            kMesh.VBuffer.GetColor4(0, iPathIndex, kNewColor);

            /* Add the new vertex, with a new index, the new normal to the
             * lists: */
            m_kNewVerts.add(new Vector3f(kNewVertex.X, kNewVertex.Y, kNewVertex.Z));
            m_kNewNormals.add(new Vector3f(kNewNormal));
            m_kNewTexCoords.add(new Vector3f(kNewTexCoord));
            m_kNewColors.add(new ColorRGBA(kNewColor));

            /* Add the new path index to the new list: */
            if (!bOpen && (kNewPath != null)) {
                kNewPath.add(new Integer(getPathIndex(kPath, iVertexCount, iPathIndex, bOpen)));
            }

            /* Get the previous path index: */
            iPrevPathIndex = (int) (kPath.get(iPath - 1)).W;

            /* Get the next path index: */
            if (iPath == (kPath.size() - 1)) {

                if (!bOpen) {
                    iNextPathIndex = (int) (kPath.get(1)).W;
                } else {
                    break;
                }
            } else {
                iNextPathIndex = (int) (kPath.get(iPath + 1)).W;
            }

            /* For each triangle that contains the edge iPrevPathIndex ->
             * iPathIndex, determine which triangle is on the "left" or
             * "right" side of the edge. */
            if (iPath == 1) {
                iNumEndPoints = findTriPoints(iPrevPathIndex, iPathIndex, kEndPoints);
                iEndIndex = (kEndPoints.get(0)).intValue();

                if (iNumEndPoints == 2) {

                    if (onRight(kMesh, iPrevPathIndex, iPathIndex, (kEndPoints.get(0)).intValue())) {
                        iEndIndex = (kEndPoints.get(0)).intValue();
                    } else {
                        iEndIndex = (kEndPoints.get(1)).intValue();
                    }
                }
            }

            kEndList.add(new Integer(iEndIndex));

            while ((iEndIndex != iNextPathIndex) && (iEndIndex != iPrevPathIndex)) {

                /* Add to the Add/Remove list: */
                kDeleteTri.set(iPrevPathIndex, iPathIndex, iEndIndex);

                if (triangleExists(kDeleteTri)) {
                    if (!contains(m_kRemoveTriangles, kDeleteTri)) {
                        m_kRemoveTriangles.add(new Vector3f(kDeleteTri));
                    }

                    kAddTri.X = getPathIndex(kPath, iVertexCount, iPrevPathIndex, bOpen);
                    kAddTri.Y = getPathIndex(kPath, iVertexCount, iPathIndex, bOpen);
                    kAddTri.Z = getPathIndex(kPath, iVertexCount, iEndIndex, bOpen);

                    if (!contains(m_kNewTriangles, kAddTri)) {
                        m_kNewTriangles.add(new Vector3f(kAddTri));
                    }
                }

                /* Move to the next side triangle index: */
                iNumEndPoints = findTriPoints(iEndIndex, iPathIndex, kEndPoints);

                if (iNumEndPoints == 0) {
                    break;
                }

                if (iNumEndPoints == 1) {
                    break;
                }

                for (int iEnd = 0; iEnd < iNumEndPoints; iEnd++) {

                    if ((kEndPoints.get(iEnd)).intValue() != iPrevPathIndex) {
                        iPrevPathIndex = iEndIndex;
                        iEndIndex = (kEndPoints.get(iEnd)).intValue();

                        if (kEndList.contains(new Integer(iEndIndex))) {
                            iEndIndex = iNextPathIndex;
                        }

                        kEndList.add(new Integer(iEndIndex));

                        break;
                    }
                }
            }

            kEndList.clear();
            iEndIndex = iPrevPathIndex;

            if ((iPath == (kPath.size() - 2)) && bOpen) {

                /* Add to the Add/Remove list: */
                kDeleteTri.set(iPathIndex, iNextPathIndex, iEndIndex);

                if (triangleExists(kDeleteTri)) {
                    if (!contains(m_kRemoveTriangles, kDeleteTri)) {
                        m_kRemoveTriangles.add(new Vector3f(kDeleteTri));
                    }

                    kAddTri.X = getPathIndex(kPath, iVertexCount, iPathIndex, bOpen);
                    kAddTri.Y = getPathIndex(kPath, iVertexCount, iNextPathIndex, bOpen);
                    kAddTri.Z = getPathIndex(kPath, iVertexCount, iEndIndex, bOpen);

                    if (!contains(m_kNewTriangles, kAddTri)) {
                        m_kNewTriangles.add(new Vector3f(kAddTri));
                    }
                }
            }
        }

        kAddTri = null;
        kDeleteTri = null;
        kNewNormal = null;
        kNewVertex = null;
        kEndList.clear();
        kEndList = null;

        return m_kNewVerts.size();
    }
}
