package gov.nih.mipav.view.renderer.J3D.surfaceview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;

import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.picking.*;

import java.awt.event.*;

import java.util.*;

import javax.media.j3d.*;

import javax.swing.*;

import javax.vecmath.*;


/**
 * Introduction: The goal of this task is to add the ability for the user to draw a region of interest on a triangle
 * mesh surface, by allowing the user to select points on the mesh surface and calculating the connecting polyline on
 * the surface between the points. Pairs of consecutive points are connected by a geodesics path. The geodesic path is
 * the shortest surface path between the points. For a triangle mesh, the final boundary curve for the region of
 * interest is a polyline. To compute the polyline curve I start with Dijkstra's Single-Source Shortest Path graph
 * algorithm to find the shortest path along triangle edges between the pair of points on the mesh. In the algorithm,
 * the triangle vertices and edges are the nodes and edges of the graph, and the Euclidean distance between vertices
 * connected by an edge serves as the edge weight. The result of Dijkstra's algorithm is a path that follows the edges
 * in the mesh and may be jagged. I then iterate over the points on the polyline curve and the edges of the triangle
 * mesh the curve crosses, to compute new points on the curve that lie on the triangle edges but not necessarily on the
 * triangle vertices. This produces a smoother curve. Implementation: Drawing the user-selected region of interest on a
 * surface is implemented through the Geodesic class in Geodesic.java. The Geodesic class may be used in two different
 * ways. It may be used as a MouseListener that implements picking points on the surface of the mesh with the mouse and
 * drawing the region of interest directly on the mesh. Or it may be used just to calculate the geodesic curve between
 * two points on the triangle mesh. When the Geodesic class is used just to calculate the geodesic curve between two
 * points on the triangle mesh, the points on the mesh must be specified prior to calculation. The points may be
 * determined through a different picking implementation, or may be determined some other way. The interface to the
 * Geodesic class allows the programmer to set the end points directly and to read the resulting geodesic curve.
 * Calculating the Geodesic Curve The first step in calculating the geodesic curve on the triangle mesh, whether the
 * Geodesic class implements MouseListener or not, is to initialize the local copy of the triangle mesh and the
 * Single-Source Shortest Path Algorithm data structures based on the locations of the two endpoints of the path. The
 * two endpoints of the path are specified in local mesh coordinates. They are not restricted to vertices in the
 * triangle mesh, but may fall inside a triangle. When the endpoints fall inside a triangle, the triangle is divided
 * into three new triangles, by connecting the new vertex to each of the three triangle vertices. This modification only
 * affects the local copy of the triangle mesh. In Dijkstra's Single-Source Shortest Path Algorithm, each vertex and
 * edge in the mesh has a weight; vertex weights represent the distance from the source vertex to that vertex, edge
 * weights represent the distance between the two vertices the edge connects. In this implementation of Dijkstra's
 * algorithm the edge weight is the Euclidean distance between the two vertices. Optimization: At the start of the
 * algorithm each vertex is initialized to Float.MAX_VALUE, to represent the distance from the start node to that node
 * has not yet been determined. Because the nodes and edges fall on a triangle mesh, where distances are based on actual
 * distances in 3-dimensions, we can use an optimization to the standard Dijkstra's algorithm. This optimization uses an
 * additional weight factor, which is the straight-line distance from the vertex to the end vertex. This is calculated
 * for each vertex in the triangle mesh. Dijkstra's algorithm is a Greedy Algorithm. As it proceeds, points are
 * examined, "relaxed", and potentially added to the final path based on the distance from the start vertex - the vertex
 * not yet on the shortest path with the lowest weight is examined first. In the optimized version, the vertex with the
 * lowest weight plus the lowest straight-line distance to the end point is examined, or "relaxed" first. This causes
 * the area of vertices that Dijkstra's algorithm searches to be weighted in the direction of the end vertex. A
 * non-optimized search pattern is symmetrical, and spreads out in a spherical pattern around the start vertex. The
 * optimized search pattern appears conical, and points in the direction of the end vertex. Dijkstra's algorithm returns
 * a list of points on the triangle mesh that connect the start and end vertices. The path travels along edges in the
 * mesh and so may appear jagged. The final step in the Geodesic class algorithm is to smooth the polyline by moving
 * points that fall on triangle vertices along the triangle edges. The final polyline contains points that are on
 * triangle mesh edges, but are not constrained to the triangle vertices. The smoothing process is iterative. Given two
 * points in the polyline that are connected by a path that goes through one triangle vertex, the intermediate vertex is
 * moved across the edges that extend from it, until a new position is found that minimizes the distance between the two
 * points. If more than one edge is crossed, then a new vertex for each new edge crossed is added to the polyline. The
 * smoothing proceeds for each pair of points on the polyline. Added 7/31/05: LiveWire mode. LiveWire mode enables the
 * user to interactively watch the Dijkstra's path being drawn between the last point placed on the curve to the current
 * mouse location. LiveWire mode does not compute the smoothed geodesic interactively, but instead, waits for the user
 * to place points on Dijkstra's curve and then finish the curve -- by pressing either the "Finish Open" or "Finish
 * Closed" buttons in the interface. Once the curve is finished, then the smoothed geodesic is calculated and the
 * triangle mesh is re-triangulated along the curve. Added: Cutting the mesh along the Geodesic.
 *
 * @author  Alexandra Bokinsky, Ph.D. Under contract from Magic Software.
 * @see     ViewJFrameVolumeView
 */
public class Geodesic implements MouseListener, MouseMotionListener, KeyListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Reference to the JPanelGeodesic:. */
    JPanelGeodesic m_kPanel = null;

    /** DOCUMENT ME! */
    private boolean[] m_abRemoveTris;

    /** DOCUMENT ME! */
    private int[] m_aiEndIndex = null;

    /** DOCUMENT ME! */
    private int[] m_aiFirstIndex = null;

    /** vertices array. */
    private int[] m_aiIndex = null;

    /** DOCUMENT ME! */
    private int[] m_aiIndexShift;

    /** DOCUMENT ME! */
    private int[] m_aiPreviousStartIndex = null;

    /**
     * The index values in the Vertex array for the triangle that the Start, First, and End points fall in, if they are
     * inside a triangle and not on a triangle vertex:.
     */
    private int[] m_aiStartIndex = null;

    /** triangle index coordinate array. */
    private Point3f[] m_akCoordinates = null;

    /**
     * Data members used in Dijkstra's search. The Edgelist is a list for each vertex of all the vertices that it is
     * connected to by one edge. The vertices are stored in the Edgelist as the vertex index.
     */
    private LinkedList[] m_akEdgeList = null;

    /** triangle normal array. */
    private Vector3f[] m_akNormals;

    /** triangle texture coordinate array. */
    private TexCoord3f[] m_akTexCoords;

    /** triangle color array. */
    private Color4f[] m_akColors;

    /** toggle for displaying Dijkstra's path as well as the smoothed path:. */
    private boolean m_bDisplayDijkstra = false;

    /** Turned on when picking with the mouse is enabled:. */
    private boolean m_bEnabled = false;

    /** Flag to indicates end ponit changes. */
    private boolean m_bEndpointChanged = false;

    /** Closing the Geodesic path:. */
    private boolean m_bFinished = true;

    /** DOCUMENT ME! */
    private boolean m_bFirstWire = false;

    /** Flag for clearing the Geodesic curves, if it is false, no curves have been added to the GeodesicGroup. */
    private boolean m_bGroupAdded = false;

    /** DOCUMENT ME! */
    private boolean m_bLastWire = false;

    /** Live wire or point and click mode:. */
    private boolean m_bLivewire = false;

    /** DOCUMENT ME! */
    private boolean m_bMouseMotion = false;

    /**
     * Mouse events. Setting mousePressed and mouseReleased explicitly when the mouse events are received has deals with
     * getting multiply mouse event notifications for the same mouse press.
     */
    private boolean m_bMousePressed = false;

    /** DOCUMENT ME! */
    private boolean m_bMouseReleased = true;

    /** Close path or not. */
    private boolean m_bOpen = true;

    /** DOCUMENT ME! */
    private boolean[] m_bRelaxed = null;

    /** Path length statistics for each type of path:. */
    private float m_fDijkstraPathLength = 0;

    /** Error correction Epsilon. */
    private float m_fEpsilon = 0.0001f;

    /**
     * Radius of the sphere displayed to mark the points on the Geodesic. This can be set directly by the class using
     * the Geodesic object.
     */
    private float m_fRadius = .10f;

    /** Weights, relaxed flags, and previous vertex index for Dijkstra's search:. */
    private float[] m_fRemainingWeight = null;

    /** DOCUMENT ME! */
    private float m_fSmoothedPathLength = 0;

    /** DOCUMENT ME! */
    private float[] m_fWeight = null;

    /** DOCUMENT ME! */
    private int m_iDijkstraCount = 1;

    /** DOCUMENT ME! */
    private int m_iEnd = -1;

    /** DOCUMENT ME! */
    private int m_iFirst = -1;

    /** index count. */
    private int m_iIndexCount = 0;

    /** DOCUMENT ME! */
    private int m_iLineClosed = 0;

    /** DOCUMENT ME! */
    private int m_iNumGeodesicVertices = 0;

    /** Number of meshes. */
    private int m_iNumNewMeshes = 0;

    /** Keeps track of the of picking, so that when a pair of points has been picked the Geodesic is calculated:. */
    private int m_iNumPicked = 0;

    /** Number of triangles in the mesh, after the new triangles are added:. */
    private int m_iNumTriangles = 0;

    /** number of vertices in the path. */
    private int m_iNumWorking = 0;

    /** DOCUMENT ME! */
    private int[] m_iPrevious = null;

    /** The start, first, and end index values for the pair of points:. */
    private int m_iStart = -1;

    /**
     * Local copies of the Vertex and Index arrays: a local copy is kept so that when the start or end points fall
     * inside a triangle, a new vertex is added to the vertex array, and three new triangles are added to the triangle
     * index array, new normals are added to the Normal array:.
     */
    private int m_iVertexCount = 0;

    /** DOCUMENT ME! */
    private int m_iWhich = 0;

    /**
     * Data memebr for Dijkstra's search. The m_kBorder list stores all the vertices that have been visited by
     * Dijkstra's search, but that have not yet been relaxed. It is used to speed up the search for the non-relaxed
     * vertex with the smallest path distance
     */
    private LinkedList m_kBorder = null;

    /** DOCUMENT ME! */
    private BranchGroup m_kDijkstraGeodesicGroup = null;

    /** DOCUMENT ME! */
    private Point3f m_kEndPoint = null;

    /** DOCUMENT ME! */
    private ModelTriangleMesh m_kEndSurface;

    /** DOCUMENT ME! */
    private BranchGroup m_kEuclidianGeodesicGroup = null;

    /** DOCUMENT ME! */
    private ModelTriangleMesh m_kFinished;

    /** First point, for closing the Geodesic curve:. */
    private Point3f m_kFirstPoint = null;

    /** For finished paths, either open or closed:. */
    private LinkedList m_kGeodesic_Finished = new LinkedList();

    /**
     * LinkedLists to contain the working paths in progress, and all finished paths, open and closed for the smoothed
     * geodesics and dijkstra's geodesics. These are used in the cutting operations: For paths that are not finished,
     * points may still be added to these paths:
     */
    private LinkedList m_kGeodesic_Working = new LinkedList();

    /** DOCUMENT ME! */
    private LinkedList m_kGeodesic_Working_Left = new LinkedList();

    /** DOCUMENT ME! */
    private LinkedList m_kGeodesic_Working_Right = new LinkedList();

    /**
     * Group for drawing the Geodesic on the triangle mesh, assumes that the Group is created in the same branch tree as
     * the triangle mesh surface, so when the mesh is transformed (rotated,scaled,translated) the polyline drawn and
     * stored in the Group m_kSmoothedGeodesicGroup will be transformed in the same way:.
     */
    private Group m_kGeodesicGroup = null;

    /** The final list of points in the Geodesic curve. All points are constrained to lie on the triangle mesh, */
    private Point3f[] m_kGeodesicVertices = null;

    /** DOCUMENT ME! */
    private ModelTriangleMesh m_kLastCut;

    /** DOCUMENT ME! */
    private ModelTriangleMesh m_kLastFinished;

    /** DOCUMENT ME! */
    private ModelTriangleMesh m_kModified;

    /** DOCUMENT ME! */
    private MouseEvent m_kMouseEvent;

    /** New normal link list. */
    private LinkedList m_kNewNormals;

    /** New texCoords link list. */
    private LinkedList m_kNewTexCoords;

    /** New Colors link list. */
    private LinkedList m_kNewColors;

    /** new triangle link list. */
    private LinkedList m_kNewTriangles;

    /** New vertices link list. */
    private LinkedList m_kNewVerts;

    /** Data members for the Geodesic Class: Triangle mesh:. */
    private ModelTriangleMesh m_kOriginal;

    /** Volume renderer progress bar:. */
    private JProgressBar m_kPBar = null;

    /** PickCanvas, created by the class that creates the triangle mesh and the geodesic group:. */
    private PickCanvas m_kPickCanvas = null;

    /** Color of the first and sucessive points on the Geodesic curve:. */
    private Color3f[] m_kPickColors;

    /** DOCUMENT ME! */
    private Point3f m_kPreviousStartPoint = null;

    /** Removed triangle link list. */
    private LinkedList m_kRemoveTriangles;

    /** Root group for different path. */
    private BranchGroup m_kSmoothedGeodesicGroup = null;

    /** link list to hold the path. */
    private LinkedList m_kStartEndList = new LinkedList();

    /**
     * Start and End points -- pair of points for which a Geodesic is calculated, must be in TriangleMesh coordinates:.
     */
    private Point3f m_kStartPoint = null;

    /** DOCUMENT ME! */
    private ModelTriangleMesh m_kStartSurface;

    /** DOCUMENT ME! */
    private ModelTriangleMesh m_kSurface;

    /** DOCUMENT ME! */
    private ModelTriangleMesh m_kSurfaceBackup;

    /** DOCUMENT ME! */
    private Switch m_kSwitchDisplay = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Instantiation without initializing the progress bar, pickCanvas, GeodesicGroup, triangle mesh or sphere radius,
     * each of those can be set through individual member access functions:.
     */
    public Geodesic() {
        initColors();
    }

    /**
     * Instantiaion of the Geodesic object, with the objects necessary for the Geodesic to serve as a MouseListener that
     * performs picking and with the Group kGeodesicGroup so that the Geodesic curve can be drawn directly on the
     * ModelTriangleMesh.
     *
     * @param  kPickCanvas     PickCanvas
     * @param  kGeodesicGroup  Group
     * @param  kMesh           ModelTriangleMesh surface
     * @param  fRadius         float marker sphere radius
     */
    public Geodesic(PickCanvas kPickCanvas, Group kGeodesicGroup, ModelTriangleMesh kMesh, float fRadius) {
        m_kPBar = ViewJFrameVolumeView.getRendererProgressBar();

        setPickCanvas(kPickCanvas);
        setGeodesicGroup(kGeodesicGroup);
        setSurface(kMesh);
        setRadius(fRadius);

        initColors();
        m_kPickColors = new Color3f[2];
        m_kPickColors[0] = new Color3f(1, 0, 0);
        m_kPickColors[1] = new Color3f(1, 1, 0);
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

                if (!m_kStartPoint.equals(m_kPreviousStartPoint)) {

                    if (!m_bLastWire) {
                        iNode = m_kSmoothedGeodesicGroup.numChildren() - 1;

                        if (iNode >= 0) {
                            m_kSmoothedGeodesicGroup.removeChild(iNode);
                            m_kDijkstraGeodesicGroup.removeChild(iNode);
                            m_kEuclidianGeodesicGroup.removeChild(iNode);
                            iNumRemoved++;
                        }

                        /* If the last iNode is the line connecting the last point
                         * to the first point, closing the curve, so do not delete
                         * the last point, just delete the connecting geodesic: */
                        if ((iNode > 0) && (iNode != (m_iLineClosed + 1))) {
                            iNode--;
                            m_kSmoothedGeodesicGroup.removeChild(iNode);
                            m_kDijkstraGeodesicGroup.removeChild(iNode);
                            m_kEuclidianGeodesicGroup.removeChild(iNode);
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
                        iNode = m_kDijkstraGeodesicGroup.numChildren() - 1;

                        /* Remove the last point and the last curve: */
                        m_kDijkstraGeodesicGroup.removeChild(iNode);
                        m_kEuclidianGeodesicGroup.removeChild(iNode);
                        iNumRemoved++;
                        iNode = m_kDijkstraGeodesicGroup.numChildren() - 1;
                        m_kDijkstraGeodesicGroup.removeChild(iNode);
                        m_kEuclidianGeodesicGroup.removeChild(iNode);
                        iNumRemoved++;

                        clearLastStartEnd();
                    }
                }
            }
            /* Clear all points and curves, and disable drawing: */
            else {
                m_bEnabled = false;
                m_kSmoothedGeodesicGroup.removeAllChildren();
                m_kDijkstraGeodesicGroup.removeAllChildren();
                m_kEuclidianGeodesicGroup.removeAllChildren();

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
     * @param   bSmoothed    flag to smooths and stores the shortest path.
     *
     * @return  DOCUMENT ME!
     */
    public boolean computeGeodesic(float fPercentage, boolean bSmoothed) {

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
            relaxEdges(iSmallest);

            /* Find the vertex with the smallest weight and add it to the
             * path: */
            iSmallest = findSmallest();
        }

        /* Set the progress bar indicator: */
        if (m_kPBar != null) {
            m_kPBar.setValue(100);
            m_kPBar.update(m_kPBar.getGraphics());
        }

        /* Smooths and stores the shortest path: */
        if (bSmoothed) {
            return createPath(m_iStart, m_iEnd);
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
    public boolean createPath(int iStart, int iEnd) {

        /* If the start == end, return 0 - no path: */
        if (iStart == iEnd) {
            return true;
        }

        /* The new path will have an unknown number of points, use a
         * LinkedList: */
        LinkedList kMiddle = new LinkedList();

        /* Store the End index: */
        kMiddle.add(new Integer(iEnd));

        /* kLeft and kRight store the endpoints of the edges that the path
         * intersects, the size of Left and Right should match the size of
         * kNewVert list, also LinkedLists: */
        LinkedList kLeft = new LinkedList();
        LinkedList kRight = new LinkedList();

        /* Add "unknown" for left and right for the iEnd: */
        kLeft.add(0, new Integer(-1));
        kRight.add(0, new Integer(-1));


        /* The next step is to loop over the poins in Dijkstra's path. For all
         * points along the path, except the first and last points, create a list of edges that extend to the left and
         * right of that point. If there are unequal numbers of left and right edges, then repeat the last edge for the
         * side with fewer edges:
         */
        LinkedList kEndPoints = new LinkedList();
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

                    kRight.add(new Integer(((Integer) kEndPoints.get(0)).intValue()));
                    iRightPrev = ((Integer) kEndPoints.get(0)).intValue();
                } else {

                    /* Add to left list: */
                    if (kLeft.getLast().equals(kEndPoints.get(0))) {
                        kRight.add(new Integer(-1));
                        iRightPrev = -1;
                        bRightDone = true;

                        kLeft.add(new Integer(((Integer) kEndPoints.get(0)).intValue()));
                        iLeftPrev = ((Integer) kEndPoints.get(0)).intValue();
                    }
                    /* Add to right list: */
                    else if (kRight.getLast().equals(kEndPoints.get(0))) {
                        kLeft.add(new Integer(-1));
                        iLeftPrev = -1;
                        bLeftDone = true;

                        kRight.add(new Integer(((Integer) kEndPoints.get(0)).intValue()));
                        iRightPrev = ((Integer) kEndPoints.get(0)).intValue();
                    } else {
                        kLeft.add(new Integer(-1));
                        iLeftPrev = -1;
                        bLeftDone = true;

                        kRight.add(new Integer(((Integer) kEndPoints.get(0)).intValue()));
                        iRightPrev = ((Integer) kEndPoints.get(0)).intValue();
                    }
                }
            } else {

                if ((kLeft.size() == 0) && (kRight.size() == 0)) {
                    kLeft.add(new Integer(((Integer) kEndPoints.get(0)).intValue()));
                    iLeftPrev = iNode;

                    kRight.add(new Integer(((Integer) kEndPoints.get(1)).intValue()));
                    iRightPrev = iNode;
                } else {

                    if (kLeft.getLast().equals(kEndPoints.get(0)) || kRight.getLast().equals(kEndPoints.get(1))) {
                        kLeft.add(new Integer(((Integer) kEndPoints.get(0)).intValue()));
                        iLeftPrev = iNode;

                        kRight.add(new Integer(((Integer) kEndPoints.get(1)).intValue()));
                        iRightPrev = iNode;
                    } else if (kRight.getLast().equals(kEndPoints.get(0)) ||
                                   kLeft.getLast().equals(kEndPoints.get(1))) {
                        kRight.add(new Integer(((Integer) kEndPoints.get(0)).intValue()));
                        iRightPrev = iNode;

                        kLeft.add(new Integer(((Integer) kEndPoints.get(1)).intValue()));
                        iLeftPrev = iNode;
                    } else {
                        kLeft.add(new Integer(((Integer) kEndPoints.get(0)).intValue()));
                        iLeftPrev = iNode;

                        kRight.add(new Integer(((Integer) kEndPoints.get(1)).intValue()));
                        iRightPrev = iNode;
                    }
                }
            }

            kMiddle.add(new Integer(iPrevious));

            if (bLeftDone && bRightDone) {
                bDone = true;
            }

            while (!bDone) {
                iNode = ((Integer) kLeft.getLast()).intValue();

                if (iNode != -1) {
                    iNumEndPoints = findTriPoints(iNode, iPrevious, kEndPoints);

                    if (iNumEndPoints == 1) {
                        kLeft.removeLast();
                        kLeft.add(new Integer(-1));
                        kLeft.add(new Integer(-1));
                        iLeftPrev = -1;
                        bLeftDone = true;
                    } else if (((Integer) kEndPoints.get(0)).intValue() != iLeftPrev) {

                        if (((Integer) kEndPoints.get(0)).intValue() != iPreviousPrev) {
                            kLeft.add(new Integer(((Integer) kEndPoints.get(0)).intValue()));
                            iLeftPrev = iNode;
                        } else {
                            kLeft.add(new Integer(((Integer) kLeft.getLast()).intValue()));
                            bLeftDone = true;
                        }
                    } else if (((Integer) kEndPoints.get(1)).intValue() != iPreviousPrev) {
                        kLeft.add(new Integer(((Integer) kEndPoints.get(1)).intValue()));
                        iLeftPrev = iNode;
                    } else {
                        kLeft.add(new Integer(((Integer) kLeft.getLast()).intValue()));
                        bLeftDone = true;
                    }
                } else {
                    kLeft.add(new Integer(-1));
                    iLeftPrev = -1;
                    bLeftDone = true;
                }

                iNode = ((Integer) kRight.getLast()).intValue();

                if (iNode != -1) {
                    iNumEndPoints = findTriPoints(iNode, iPrevious, kEndPoints);

                    if (iNumEndPoints == 1) {
                        kRight.removeLast();
                        kRight.add(new Integer(-1));
                        kRight.add(new Integer(-1));
                        iRightPrev = -1;
                        bRightDone = true;
                    } else if (((Integer) kEndPoints.get(0)).intValue() != iRightPrev) {

                        if (((Integer) kEndPoints.get(0)).intValue() != iPreviousPrev) {
                            kRight.add(new Integer(((Integer) kEndPoints.get(0)).intValue()));
                            iRightPrev = iNode;
                        } else {
                            kRight.add(new Integer(((Integer) kRight.getLast()).intValue()));
                            bRightDone = true;
                        }
                    } else if (((Integer) kEndPoints.get(1)).intValue() != iPreviousPrev) {
                        kRight.add(new Integer(((Integer) kEndPoints.get(1)).intValue()));
                        iRightPrev = iNode;
                    } else {
                        kRight.add(new Integer(((Integer) kRight.getLast()).intValue()));
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

        LinkedList kNewVert = new LinkedList();
        LinkedList kNewLeft = new LinkedList();
        LinkedList kNewRight = new LinkedList();

        /* Store the End index: */
        Point4f kNewPoint4 = new Point4f();
        kNewPoint4.x = m_akCoordinates[iEnd].x;
        kNewPoint4.y = m_akCoordinates[iEnd].y;
        kNewPoint4.z = m_akCoordinates[iEnd].z;
        kNewPoint4.w = iEnd;
        kNewVert.add(new Point4f(kNewPoint4));

        /* Add "unknown" for left and right for the iEnd: */
        kNewLeft.add(new Integer(-1));
        kNewRight.add(new Integer(-1));


        LinkedList kNewVertTemp = new LinkedList();
        LinkedList kLeftTemp = new LinkedList();
        LinkedList kRightTemp = new LinkedList();
        int iPrevSide = 0;
        int iPath = 1;

        while (iPath < (kMiddle.size() - 1)) {
            int iSide = smoothPath(iPath, kLeft, kMiddle, kRight, kLeftTemp, kRightTemp, kNewVertTemp);

            if ((iPath == 1) || (iPrevSide == -1)) {
                iPrevSide = iSide;
            }

            if (iSide != -1) {

                if (iSide != iPrevSide) {
                    kNewLeft.add(new Integer(((Integer) kMiddle.get(iPath - 1)).intValue()));
                    kNewRight.add(new Integer(((Integer) kMiddle.get(iPath)).intValue()));

                    int iCoord = ((Integer) kMiddle.get(iPath)).intValue();
                    kNewVert.add(new Point4f(m_akCoordinates[iCoord].x, m_akCoordinates[iCoord].y,
                                             m_akCoordinates[iCoord].z, iCoord));
                }
            }

            for (int j = 0; j < kNewVertTemp.size(); j++) {
                kNewVert.add(new Point4f((Point4f) kNewVertTemp.get(j)));
                kNewLeft.add(new Integer(((Integer) kLeftTemp.get(j)).intValue()));
                kNewRight.add(new Integer(((Integer) kRightTemp.get(j)).intValue()));
            }

            kLeftTemp.clear();
            kRightTemp.clear();
            kNewVertTemp.clear();

            while (((Integer) kMiddle.get(iPath)).intValue() == ((Integer) kMiddle.get(iPath + 1)).intValue()) {
                iPath++;
            }

            iPath++;
            iPrevSide = iSide;
        }

        kNewPoint4.x = m_akCoordinates[iStart].x;
        kNewPoint4.y = m_akCoordinates[iStart].y;
        kNewPoint4.z = m_akCoordinates[iStart].z;
        kNewPoint4.w = iStart;
        kNewVert.add(new Point4f(kNewPoint4));

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
                Point4f newPoint1 = new Point4f();
                Point4f kStart = (Point4f) (kNewVert.get(i - 1));
                Point4f kEnd = (Point4f) (kNewVert.get(i + 1));

                int iMiddle = ((Integer) kLeft.get(i)).intValue();
                int iSide = ((Integer) kRight.get(i)).intValue();

                if ((iMiddle != -1) && (iSide != -1)) {
                    findMin(new Point3f(kStart.x, kStart.y, kStart.z), iMiddle, iSide,
                            new Point3f(kEnd.x, kEnd.y, kEnd.z), newPoint1);
                    ((Point4f) kNewVert.get(i)).x = newPoint1.x;
                    ((Point4f) kNewVert.get(i)).y = newPoint1.y;
                    ((Point4f) kNewVert.get(i)).z = newPoint1.z;
                    ((Point4f) kNewVert.get(i)).w = newPoint1.w;

                    newPoint1 = null;
                }
            }
        }

        iPath = 1;

        while (iPath < (kNewVert.size() - 1)) {
            boolean bRemoved = false;
            int iCurrentPoint = (int) ((Point4f) kNewVert.get(iPath)).w;

            if (iCurrentPoint == -1) {
                int iPrevPoint = (int) ((Point4f) kNewVert.get(iPath - 1)).w;
                int iNextPoint = (int) ((Point4f) kNewVert.get(iPath + 1)).w;

                if ((((Integer) kLeft.get(iPath)).intValue() == iPrevPoint) ||
                        (((Integer) kRight.get(iPath)).intValue() == iPrevPoint)) {
                    kNewVert.remove(iPath);
                    kLeft.remove(iPath);
                    kRight.remove(iPath);
                    bRemoved = true;
                } else if ((((Integer) kLeft.get(iPath)).intValue() == iNextPoint) ||
                               (((Integer) kRight.get(iPath)).intValue() == iNextPoint)) {
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
        m_kGeodesicVertices = new Point3f[m_iNumGeodesicVertices];

        for (int iVertex = 0; iVertex < m_iNumGeodesicVertices; iVertex++) {
            m_kGeodesicVertices[iVertex] = new Point3f(((Point4f) kNewVert.get(iVertex)).x,
                                                       ((Point4f) kNewVert.get(iVertex)).y,
                                                       ((Point4f) kNewVert.get(iVertex)).z);

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

        m_kNewVerts = new LinkedList();
        m_kNewNormals = new LinkedList();
        m_kNewTexCoords = new LinkedList();
        m_kNewColors = new LinkedList();
        m_kNewTriangles = new LinkedList();
        m_kRemoveTriangles = new LinkedList();

        /* Backup the last finished mesh so it can be restored if the cut is
         * undone: */
        m_kLastCut = m_kLastFinished;

        /* Create the edgeList data for this mesh: */
        if (!createEdgeLists(m_kModified)) {
            MipavUtil.displayError("Mesh Cut failed 1, clearing cut");
            clearCut(false);

            return;
        }

        int iVertexCount = m_kModified.getVertexCount();
        int iOldVertexCount = m_kModified.getVertexCount();

        LinkedList kGeodesic_Separate = new LinkedList();

        /* First, "unzip" all open paths & remove: */
        if (m_bOpen == true) {
            LinkedList kOpenPath = (LinkedList) m_kGeodesic_Finished.get(0);
            iVertexCount += unZip(m_kModified, kOpenPath, iVertexCount, true, null);
        } else {

            /* Second, "unzip" all closed paths, storing the new path vertices: */
            LinkedList kClosedPath = (LinkedList) m_kGeodesic_Finished.get(0);
            LinkedList kNewClosedPath = new LinkedList();
            iVertexCount += unZip(m_kModified, kClosedPath, iVertexCount, false, kNewClosedPath);
            kGeodesic_Separate.add(kNewClosedPath);
        }


        ModelTriangleMesh kCutMesh = createNewMesh(m_kModified, iVertexCount, iOldVertexCount);

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

            iVertexCount = kCutMesh.getVertexCount();
            iOldVertexCount = iVertexCount;
            m_aiIndexShift = null;
            m_abRemoveTris = null;

            int iNumVertsRemoved = findNewMeshes(kCutMesh, kGeodesic_Separate);

            if (iNumVertsRemoved < m_iVertexCount) {
                iVertexCount -= iNumVertsRemoved;
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
            m_kSurfaceBackup = new ModelTriangleMesh(m_kSurface ); 
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

        m_kSmoothedGeodesicGroup = null;
        m_kDijkstraGeodesicGroup = null;
        m_kEuclidianGeodesicGroup = null;
        m_kSwitchDisplay = null;

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
    public void drawDijkstraEuclidianPath(int iStart, int iEnd) {

        /* Check that the line exists, this is more important in livewire
         * mode, where the mouse motion may cause the start and end points to
         * be the same: */
        if (iStart == iEnd) {
            return;
        }

        if (m_bLivewire && !m_bFirstWire) {
            m_kDijkstraGeodesicGroup.removeChild(m_kDijkstraGeodesicGroup.numChildren() - 1);
            m_kEuclidianGeodesicGroup.removeChild(m_kEuclidianGeodesicGroup.numChildren() - 1);
        }

        if (m_bEndpointChanged) {
            m_kDijkstraGeodesicGroup.removeChild(m_kDijkstraGeodesicGroup.numChildren() - 1);
            m_kEuclidianGeodesicGroup.removeChild(m_kEuclidianGeodesicGroup.numChildren() - 1);

            drawDijkstraEuclidianPoint(m_kEndPoint, m_kPickColors[m_iNumPicked % 2]);
        }

        /* Recreate and Display Dijkstra's Path: */
        /* Cound the original mesh points, different from the smoothed mesh
         * points: */
        int iNode = iEnd;
        int iCount = 1;
        m_fDijkstraPathLength = 0;

        while (iNode != iStart) {

            if (iNode != iStart) {
                m_fDijkstraPathLength += distance(m_akCoordinates[iNode], m_akCoordinates[m_iPrevious[iNode]]);
            }

            iNode = m_iPrevious[iNode];
            iCount++;
        }

        /* Create a new LineArray to store the displayed line: */
        LineArray kDijkstraLineArray = new LineArray(2 * (iCount - 1),
                                                     GeometryArray.COORDINATES | GeometryArray.COLOR_3);

        /* Copy the mesh points into the LineArray: */
        iNode = iEnd;
        iCount = 0;

        Color3f kColor = new Color3f(1, 1, 0);
        kDijkstraLineArray.setCoordinate(iCount, m_akCoordinates[iNode]);
        kDijkstraLineArray.setColor(iCount, kColor);
        iCount++;

        int iListCount = 0;

        while (iNode != iStart) {
            iNode = m_iPrevious[iNode];
            kDijkstraLineArray.setCoordinate(iCount, m_akCoordinates[iNode]);
            kDijkstraLineArray.setColor(iCount, kColor);
            iCount++;

            if (iNode != iStart) {
                kDijkstraLineArray.setCoordinate(iCount, m_akCoordinates[iNode]);
                kDijkstraLineArray.setColor(iCount, kColor);
                iCount++;
            }
        }

        /* Setup the shape, transform group and branchgroup for displaying the
         * LineArray: */
        Shape3D kDijkstraPathShape = new Shape3D();
        kDijkstraPathShape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        kDijkstraPathShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        kDijkstraPathShape.setCapability(Shape3D.ALLOW_BOUNDS_READ);
        kDijkstraPathShape.addGeometry(kDijkstraLineArray);
        kDijkstraPathShape.setPickable(false);

        /* Create the Transform and Branchgroups for the Line: */
        Transform3D kDijkstraTransform = new Transform3D();
        TransformGroup kDijkstraTransformGroup = new TransformGroup();
        kDijkstraTransformGroup.setTransform(kDijkstraTransform);
        kDijkstraTransformGroup.addChild(kDijkstraPathShape);

        BranchGroup kDijkstraBranchGroup = new BranchGroup();
        kDijkstraBranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
        kDijkstraBranchGroup.addChild(kDijkstraTransformGroup);
        kDijkstraBranchGroup.compile();
        m_kDijkstraGeodesicGroup.addChild(kDijkstraBranchGroup);


        /* Setup the LineArray and Shapes for the Euclidian path: */
        LineArray kEuclidianLineArray = new LineArray(2, GeometryArray.COORDINATES | GeometryArray.COLOR_3);
        kColor = new Color3f(0, 1, 1);
        kEuclidianLineArray.setCoordinate(0, m_akCoordinates[iStart]);
        kEuclidianLineArray.setColor(0, kColor);
        kEuclidianLineArray.setCoordinate(1, m_akCoordinates[iEnd]);
        kEuclidianLineArray.setColor(1, kColor);

        Shape3D kEuclidianPathShape = new Shape3D();
        kEuclidianPathShape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        kEuclidianPathShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        kEuclidianPathShape.setCapability(Shape3D.ALLOW_BOUNDS_READ);
        kEuclidianPathShape.addGeometry(kEuclidianLineArray);
        kEuclidianPathShape.setPickable(false);

        /* Create the Transform and Branchgroups for the Line: */
        Transform3D kEuclidianTransform = new Transform3D();
        TransformGroup kEuclidianTransformGroup = new TransformGroup();
        kEuclidianTransformGroup.setTransform(kEuclidianTransform);
        kEuclidianTransformGroup.addChild(kEuclidianPathShape);

        BranchGroup kEuclidianBranchGroup = new BranchGroup();
        kEuclidianBranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
        kEuclidianBranchGroup.addChild(kEuclidianTransformGroup);
        kEuclidianBranchGroup.compile();
        m_kEuclidianGeodesicGroup.addChild(kEuclidianBranchGroup);


        /* Update the panel display with the line distance information for all
         * three types of lines, geodesic, dijkstra, and euclidian: */
        if (m_kPanel != null) {
            float fEuclidian = distance(m_akCoordinates[iStart], m_akCoordinates[iEnd]);

            m_kPanel.setEuclidian(fEuclidian);
            m_kPanel.setGeodesicSmooth(m_fSmoothedPathLength);
            m_kPanel.setDijkstra(m_fDijkstraPathLength);
            m_kPanel.enableClearLast(true);
        }

        /* Delete local variables: */
        kColor = null;
    }

    /**
     * drawPath draws the Geodesic path as a LineArray and adds it to the children of the m_kSmoothedGeodesicGroup
     * object, it also draws the Dijkstra path and Euclidian paths and adds them to the corresponding
     * m_kDijkstraGeodesicGroup and m_kEuclidianGeodesicGroups.
     *
     * @param  iStart  int Geodesic path starting point
     * @param  iEnd    int Geodesic path ending point
     */
    public void drawGeodesicPath(int iStart, int iEnd) {

        /* Check that the line exists, this is more important in livewire
         * mode, where the mouse motion may cause the start and end points to
         * be the same: */
        if (iStart == iEnd) {
            return;
        }

        if (m_bEndpointChanged) {
            m_kSmoothedGeodesicGroup.removeChild(m_kSmoothedGeodesicGroup.numChildren() - 1);
            drawGeodesicPoint(m_kEndPoint, m_kPickColors[m_iNumPicked % 2]);
        }

        /* Create the Smoothed Geodesic line array: */
        LineArray kSmoothedLineArray = new LineArray(2 * (m_iNumGeodesicVertices - 1),
                                                     GeometryArray.COORDINATES | GeometryArray.COLOR_3);

        /* Color the line: */
        Color3f kColor = new Color3f(1, 0, 0);
        int iPathPointCount = 0;

        /* Add points to the line array: */
        for (int iPathNode = 0; iPathNode < m_iNumGeodesicVertices; iPathNode++) {
            kSmoothedLineArray.setCoordinate(iPathPointCount, m_kGeodesicVertices[iPathNode]);

            kSmoothedLineArray.setColor(iPathPointCount, kColor);
            iPathPointCount++;

            if ((iPathNode != 0) && (iPathNode != (m_iNumGeodesicVertices - 1))) {
                kSmoothedLineArray.setCoordinate(iPathPointCount, m_kGeodesicVertices[iPathNode]);
                kSmoothedLineArray.setColor(iPathPointCount, kColor);
                iPathPointCount++;
            }
        }

        /* Create the Shape3D object to contain the LineArray: */
        Shape3D kSmoothedPathShape = new Shape3D();
        kSmoothedPathShape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        kSmoothedPathShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        kSmoothedPathShape.setCapability(Shape3D.ALLOW_BOUNDS_READ);
        kSmoothedPathShape.addGeometry(kSmoothedLineArray);
        kSmoothedPathShape.setPickable(false);

        /* Create the Transform and Branchgroups for the Line: */
        Transform3D kSmoothedTransform = new Transform3D();
        TransformGroup kSmoothedTransformGroup = new TransformGroup();
        kSmoothedTransformGroup.setTransform(kSmoothedTransform);
        kSmoothedTransformGroup.addChild(kSmoothedPathShape);

        BranchGroup kSmoothedBranchGroup = new BranchGroup();
        kSmoothedBranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
        kSmoothedBranchGroup.addChild(kSmoothedTransformGroup);
        kSmoothedBranchGroup.compile();
        m_kSmoothedGeodesicGroup.addChild(kSmoothedBranchGroup);

        /* Delete local variables: */
        kColor = null;
    }

    /**
     * drawPath draws the Geodesic path as a LineArray and adds it to the children of the m_kSmoothedGeodesicGroup
     * object, it also draws the Dijkstra path and Euclidian paths and adds them to the corresponding
     * m_kDijkstraGeodesicGroup and m_kEuclidianGeodesicGroups.
     *
     * @param  iStart  int Geodesic path starting point
     * @param  iEnd    int Geodesic path ending point
     */
    public void drawPath(int iStart, int iEnd) {
        drawGeodesicPath(iStart, iEnd);
        drawDijkstraEuclidianPath(iStart, iEnd);
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
        } else {
            m_bFinished = true;
        }

        if (m_bLastWire) {
            m_bLastWire = false;

            for (int iPath = 0; iPath < m_kStartEndList.size(); iPath++) {
                getStartEnd(iPath);

                /* Compute the Geodesic curve. The 0.99
                 * parameter is an optimization to the
                 * Dijkstra search. */
                if (computeGeodesic(0.99f, true)) {

                    /* Draw the geodesic path */
                    drawGeodesicPath(m_iStart, m_iEnd);

                    /* Triangulate path and export new mesh: */
                    triangulateMeshPath();

                    if (iPath == 0) {
                        drawGeodesicPoint(m_akCoordinates[m_iEnd], m_kPickColors[1]);
                    }

                    drawGeodesicPoint(m_akCoordinates[m_iStart], m_kPickColors[0]);

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
            if (!m_kStartPoint.equals(m_kEndPoint)) {

                /* Compute the Geodesic curve. The 0.99
                 * parameter is an optimization to the
                 * Dijkstra search. */
                if (computeGeodesic(0.99f, true)) {

                    /* Draw the geodesic path */
                    drawPath(m_iStart, m_iEnd);

                    /* Triangulate path and export new mesh: */
                    triangulateMeshPath();
                    /* Delete the temporary variables used to
                     * calculate & draw the path */
                }

                cleanUp();
            }
        }

        m_iLineClosed = m_kSmoothedGeodesicGroup.numChildren() - 1;
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
     * Access to the ith point on the Geodesic curve. All points on the geodesic curve lie on the triangle mesh.
     *
     * @param  iPoint  ith point index
     * @param  kPoint  Point3f point's coordinate
     */
    public void getPathPoint(int iPoint, Point3f kPoint) {

        if ((iPoint >= 0) && (iPoint < m_iNumGeodesicVertices)) {
            kPoint.x = m_kGeodesicVertices[iPoint].x;
            kPoint.y = m_kGeodesicVertices[iPoint].y;
            kPoint.z = m_kGeodesicVertices[iPoint].z;
        }
    }

    /**
     * Access to the all the points on the Geodesic curve. All points on the geodesic curve lie on the triangle mesh.
     *
     * @param  akPoints  Point3f[] points coordinates array
     */
    public void getPathPoints(Point3f[] akPoints) {

        for (int iPoint = 0; iPoint < m_iNumGeodesicVertices; iPoint++) {
            akPoints[iPoint].x = m_kGeodesicVertices[iPoint].x;
            akPoints[iPoint].y = m_kGeodesicVertices[iPoint].y;
            akPoints[iPoint].z = m_kGeodesicVertices[iPoint].z;
        }
    }

    /**
     * Invoked when a key has been pressed.
     *
     * @param  kEvent  KeyEvent
     */
    public void keyPressed(KeyEvent kEvent) { }

    /**
     * Invoked when a key has been released.
     *
     * @param  kEvent  KeyEvent
     */
    public void keyReleased(KeyEvent kEvent) {

        /* If livewire is active, and the control-key is released, then
         * deactivate livewire tracking and stop the dijkstra at the last saved point on the mesh -- which is set in the
         * MouseMoved
         * function. */
        if (m_bLivewire && !m_bLastWire && (kEvent.getKeyCode() == KeyEvent.VK_CONTROL)) {
            m_bMousePressed = true;
            m_bMouseReleased = false;

            /* m_bLastWire stops the livewire interaction until the
             * curve is finished: */
            m_bLastWire = true;
            m_bMouseMotion = false;

            mouseReleased(m_kMouseEvent);
        }
    }

    /**
     * Invoked when a key has been typed.
     *
     * @param  kEvent  KeyEvent
     */
    public void keyTyped(KeyEvent kEvent) { }


    /**
     * One of the overrides necessary to be a MouseListener. This function is invoked when a button has been pressed and
     * released.
     *
     * @param  kMouseEvent  the mouse event generated by a mouse clicked
     */
    public void mouseClicked(MouseEvent kMouseEvent) { }

    /**
     * mouseDragged.
     *
     * @param  kMouseEvent  MouseEvent
     */
    public void mouseDragged(MouseEvent kMouseEvent) { }

    /**
     * One of the overrides necessary to be a MouseListener. Invoked when the mouse enters a component.
     *
     * @param  kMouseEvent  the mouse event generated by a mouse entered
     */
    public void mouseEntered(MouseEvent kMouseEvent) { }

    /**
     * One of the overrides necessary to be a MouseListener. Invoked when the mouse leaves a component.
     *
     * @param  kMouseEvent  the mouse event generated by a mouse exit
     */
    public void mouseExited(MouseEvent kMouseEvent) { }


    /**
     * mouseMoved. In livewire mode the mouseMoved function updates the endpoints of the Dijkstra's geodesic and
     * computes Dijkstra's path between the last point added and the mouse position. Only Dijkstra's and the Euclidian
     * paths are updated, the smoothed geodesic is not calculated or displayed:
     *
     * @param  kMouseEvent  MouseEvent
     */
    public void mouseMoved(MouseEvent kMouseEvent) {

        /* If the mode is not livewire, or if the last point on the livewire
         * curve has been added, then do nothing: */
        if (!m_bLivewire || m_bLastWire || !m_bMouseMotion) {
            return;
        }

        m_bMouseMotion = false;

        /* If the mouse has been pressed, but not released: */
        if (m_iNumPicked > 0) {

            /* If the pickCanvas is null, then do not try to pick */
            if (m_kPickCanvas == null) {
                m_bMouseMotion = true;

                return;
            }

            /* Set the location for picking that was stored when the mouse was
             * presed: */
            m_kPickCanvas.setShapeLocation(kMouseEvent);

            PickResult kPickResult = null;

            /* Try to get the closest picked polygon, catch the
             * javax.media.j3d.CapabilityNotSetException. */
            try {
                kPickResult = m_kPickCanvas.pickClosest();
            } catch (javax.media.j3d.CapabilityNotSetException e) {
                System.err.println("pickClosest failed: " + e.getMessage());
                m_bMouseMotion = true;

                return;
            }

            /* If the pickResult is not null, mark the picked point and, if
             * this is the second point in a sequence, then draw the geodesic
             * curve. */
            if (kPickResult != null) {

                /* Pick the first intersection since we executed a pick
                 * closest. */
                PickIntersection kPick = kPickResult.getIntersection(0);

                int iClosestVertexIndex = kPick.getClosestVertexIndex();

                /* Assume this is the second point in the sequence, then draw the
                 * point and calculate the dijkstra curve connecting the two
                 * points. */
                ModelTriangleMesh kMesh = (ModelTriangleMesh) kPickResult.getGeometryArray();
                setEndSurface(kMesh);

                if (m_kStartSurface != m_kEndSurface) {
                    m_iNumPicked--;
                } else {

                    if (m_bFirstWire) {
                        setEndPoint(m_kStartPoint);
                        setEndIndices(m_aiStartIndex);
                    }

                    m_iNumPicked++;

                    /* Set start point to the new picked point: */
                    setStartIndices(kPick.getPrimitiveCoordinateIndices(), false);
                    iClosestVertexIndex = m_aiStartIndex[iClosestVertexIndex];

                    /* Get the coordinates of the picked point on the mesh,
                     * this is the closest mesh vertex, not a new vertex inside a triangle. If the picked point falls
                     * inside a triangle this is handled when the livewire curve is finished and the smoothed geodesic
                     * curve is
                     * calculated: */
                    Point3f kPickPoint = new Point3f();
                    m_kSurface.getCoordinate(iClosestVertexIndex, kPickPoint);
                    setStartPoint(kPickPoint, false);
                    m_iStart = iClosestVertexIndex;

                    /* Check that the two end points are not the
                     * same: */
                    if (!m_kStartPoint.equals(m_kEndPoint)) {

                        if (computeGeodesic(0.99f, false)) {
                            drawDijkstraEuclidianPath(m_iStart, m_iEnd);
                            m_bFinished = false;
                        }
                    }

                    /* save this start point: */
                    kPickPoint = null;
                    kPickPoint = new Point3f(kPick.getPointCoordinates());
                    setStartPoint(kPickPoint, false);
                    kPickPoint = null;

                    if (m_bFirstWire) {
                        m_bFirstWire = false;
                    }

                    /* Delete the temporary variables used to
                     * calculate & draw the path */
                    cleanUp();
                }
            }
        }

        m_bMouseMotion = true;
    }

    /**
     * One of the overrides necessary to be a MouseListener. Invoked when a mouse button is pressed.
     *
     * @param  kMouseEvent  the mouse event generated by a mouse press
     */
    public void mousePressed(MouseEvent kMouseEvent) {

        /* Only capture mouse events when drawing the geodesic curve is
         * enabled, and only when the control key is down and the left mouse
         * button is pressed. */
        if (m_bEnabled && kMouseEvent.isControlDown() && !m_bLastWire) {

            /* m_bMousePressed and m_bMouseReleased are set explicitly to
             * prevent multiple mouse clicks at the same location.  If the mouse has been released, then set
             * mousePressed to true and save the location of the mouse event.
             */
            if (m_bMouseReleased == true) {

                if ((kMouseEvent.getButton() == MouseEvent.BUTTON1)) {
                    m_bMousePressed = true;
                    m_bMouseReleased = false;

                    m_kMouseEvent = kMouseEvent;
                    m_bMouseMotion = true;
                }
                /* If the right button is pressed, and we are in livewire
                 * mode, the end the livewire interaction, allowing the user
                 * to finish the curve or to remove the last point added: */
                else if (m_bLivewire && (kMouseEvent.getButton() == MouseEvent.BUTTON3)) {
                    m_bMousePressed = true;
                    m_bMouseReleased = false;

                    /* m_bLastWire stops the livewire interaction until the
                     * curve is finished: */
                    m_bLastWire = true;
                    m_kMouseEvent = kMouseEvent;
                    m_bMouseMotion = false;
                }

                mouseReleased(kMouseEvent);
            }
        }
    }


    /**
     * One of the overrides necessary to be a MouseListener. Invoked when a mouse button is released.
     *
     * @param  kMouseEvent  the mouse event generated by a mouse release
     */
    public void mouseReleased(MouseEvent kMouseEvent) {

        /* If the mouse has been pressed, but not released: */
        if (m_bMousePressed && !m_bMouseReleased) {
            m_bMousePressed = false;
            m_bMouseReleased = true;

            /* If the pickCanvas is null, then do not try to pick */
            if (m_kPickCanvas == null) {
                return;
            }

            /* Set the location for picking that was stored when the mouse was
             * presed: */
            m_kPickCanvas.setShapeLocation(m_kMouseEvent);

            PickResult kPickResult = null;

            /* Try to get the closest picked polygon, catch the
             * javax.media.j3d.CapabilityNotSetException. */
            try {
                kPickResult = m_kPickCanvas.pickClosest();
            } catch (javax.media.j3d.CapabilityNotSetException e) {
                System.err.println("pickClosest failed: " + e.getMessage());
                m_bLastWire = false;

                return;
            }

            /* If the pickResult is not null, mark the picked point and, if
             * this is the second point in a sequence, then draw the geodesic
             * curve. */
            if (kPickResult != null) {

                /* Pick the first intersection since we executed a pick
                 * closest. */
                PickIntersection kPick = kPickResult.getIntersection(0);

                /* Get the coordinates of the picked point on the mesh. */
                Point3f kPickPoint = new Point3f(kPick.getPointCoordinates());

                /* Increment the number of picked points. */
                m_iNumPicked++;

                if (m_iNumPicked > 2) {
                    m_iNumPicked = 2;
                }

                /* If this is the first point picked in the sequence, then
                 * mark the point with a sphere */
                if (m_iNumPicked == 1) {
                    ModelTriangleMesh kMesh = (ModelTriangleMesh) kPickResult.getGeometryArray();
                    setStartSurface(kMesh);

                    setStartPoint(kPickPoint, true);
                    setStartIndices(kPick.getPrimitiveCoordinateIndices(), true);

                    /* If this s livewire mode, only draw the point in
                     * Dijkstra's and the Euclidian scene graphs: */
                    if (m_bLivewire) {
                        drawDijkstraEuclidianPoint(m_kStartPoint, m_kPickColors[m_iNumPicked % 2]);
                    }
                    /* Otherwise add the point to all three scene graphs:
                     * SmoothedGeodesic, Dijkstra, and Euclidian: */
                    else {
                        drawPoint(m_kStartPoint, m_kPickColors[m_iNumPicked % 2]);
                    }

                    /* Set first wire to be true and last wire to be false: */
                    m_bFirstWire = true;
                    m_bLastWire = false;
                }
                /* If this is the second point in the sequence, then draw the
                 * point and calculate the geodesic curve connecting the two
                 * points. */
                else if (m_iNumPicked == 2) {
                    ModelTriangleMesh kMesh = (ModelTriangleMesh) kPickResult.getGeometryArray();
                    setEndSurface(kMesh);

                    if (m_kStartSurface != m_kEndSurface) {
                        m_iNumPicked--;
                    } else {

                        /* If this is livewire mode, then the start point is
                         * already set in the mouseMove function, calculate
                         * the geodesic from there: */
                        if (m_bLivewire) {

                            if (!m_kStartPoint.equals(m_kEndPoint)) {

                                if (computeGeodesic(0.99f, false)) {
                                    saveStartEnd();
                                    drawDijkstraEuclidianPath(m_iStart, m_iEnd);
                                    m_bFinished = false;
                                    m_bFirstWire = true;
                                    drawDijkstraEuclidianPoint(m_kStartPoint, m_kPickColors[m_iNumPicked % 2]);
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
                        setStartPoint(kPickPoint, false);
                        setStartIndices(kPick.getPrimitiveCoordinateIndices(), false);

                        /* Check that the two end points are not the
                         * same: */
                        if (!m_kStartPoint.equals(m_kEndPoint)) {

                            if (!m_bLivewire) {

                                /* Compute the Geodesic curve. The 0.99
                                 * parameter is an optimization to the
                                 * Dijkstra search. */
                                if (computeGeodesic(0.99f, true)) {

                                    /* Draw the geodesic path */
                                    drawPath(m_iStart, m_iEnd);

                                    /* Triangulate path and export new mesh: */
                                    triangulateMeshPath();

                                    m_bFinished = false;

                                    drawPoint(m_kStartPoint, m_kPickColors[m_iNumPicked % 2]);
                                }
                            }
                        }

                        /* Delete the temporary variables used to
                         * calculate & draw the path */
                        cleanUp();
                    }
                }

                kPickPoint = null;
            }
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

        if ((m_bEnabled == false) && (m_kSmoothedGeodesicGroup.numChildren() > 0)) {
            m_iLineClosed = m_kSmoothedGeodesicGroup.numChildren() - 1;
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
    public void setEndPoint(Point3f kPoint) {

        if (m_kEndPoint != null) {
            m_kEndPoint = null;
        }

        m_kEndPoint = new Point3f(kPoint);
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
    public void setGeodesicGroup(Group kGeodesicGroup) {

        if (m_kSmoothedGeodesicGroup == null) {
            m_kSmoothedGeodesicGroup = new BranchGroup();
            m_kSmoothedGeodesicGroup.setCapability(BranchGroup.ALLOW_DETACH);
            m_kSmoothedGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND);
            m_kSmoothedGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);
            m_kSmoothedGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_READ);
        }

        if (m_kDijkstraGeodesicGroup == null) {
            m_kDijkstraGeodesicGroup = new BranchGroup();
            m_kDijkstraGeodesicGroup.setCapability(BranchGroup.ALLOW_DETACH);
            m_kDijkstraGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND);
            m_kDijkstraGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);
            m_kDijkstraGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_READ);
        }

        if (m_kEuclidianGeodesicGroup == null) {
            m_kEuclidianGeodesicGroup = new BranchGroup();
            m_kEuclidianGeodesicGroup.setCapability(BranchGroup.ALLOW_DETACH);
            m_kEuclidianGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND);
            m_kEuclidianGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);
            m_kEuclidianGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_READ);
        }

        if (m_kSwitchDisplay == null) {
            m_kSwitchDisplay = new Switch();
            m_kSwitchDisplay.setCapability(Switch.ALLOW_SWITCH_READ);
            m_kSwitchDisplay.setCapability(Switch.ALLOW_SWITCH_WRITE);
            m_kSwitchDisplay.setWhichChild(Switch.CHILD_ALL);
            m_kSwitchDisplay.addChild(m_kSmoothedGeodesicGroup);
            m_kSwitchDisplay.addChild(m_kDijkstraGeodesicGroup);
            m_kSwitchDisplay.addChild(m_kEuclidianGeodesicGroup);
            m_kSwitchDisplay.setWhichChild(m_iWhich);
        }

        if (m_kGeodesicGroup == null) {
            m_kGeodesicGroup = kGeodesicGroup;

            BranchGroup kBranchG = new BranchGroup();
            kBranchG.addChild(m_kSwitchDisplay);
            m_kGeodesicGroup.addChild(kBranchG);
        }
    }

    /**
     * Access to the JPanelGeodesic interface object.
     *
     * @param  kPanel  JPanelGeodesic geodesic panel
     */
    public void setPanel(JPanelGeodesic kPanel) {
        m_kPanel = kPanel;
    }


    /**
     * Access function to set the pickCanvas. This is necessary for the Geodesic class to do picking with the mouse.
     *
     * @param  kPickCanvas  PickCanvas
     */
    public void setPickCanvas(PickCanvas kPickCanvas) {
        m_kPickCanvas = kPickCanvas;
        m_kPickCanvas.getCanvas().addMouseListener(this);
        m_kPickCanvas.getCanvas().addMouseMotionListener(this);
        m_kPickCanvas.getCanvas().addKeyListener(this);
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
            m_iFirst = iIndex;
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
    public void setStartPoint(Point3f kPoint, boolean bFirst) {

        if (m_kStartPoint != null) {
            m_kStartPoint = null;
        }

        m_kStartPoint = new Point3f(kPoint);

        if (bFirst == true) {
            m_kFirstPoint = new Point3f(kPoint);
        }
    }

    /**
     * Called by the JPanelGeodesic interface to switch between displaying the Smoothed Geodesic, Dijkstra's path, or
     * the Euclidian path.
     *
     * @param  iWhich  display group index
     */
    public void toggleDisplay(int iWhich) {

        if ((m_iWhich >= 0) && (m_iWhich < 3)) {
            m_iWhich = iWhich;

            if (m_kSwitchDisplay == null) {
                return;
            }

            m_kSwitchDisplay.setWhichChild(m_iWhich);
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
     * @param   kMesh       ModelTriangleMesh surface
     * @param   kPoint      Point3f
     * @param   aiTriIndex  int[]
     * @param   kNormal     Vector3f
     *
     * @return  int
     */
    private int checkOnEdge(ModelTriangleMesh kMesh, Point3f kPoint, int[] aiTriIndex,
                            Vector3f kNormal, TexCoord3f kTexCoord, Color4f kColor )
    {
        int iReturn = -1;
        m_fEpsilon *= 2f;

        Point3f kTri0 = new Point3f();
        m_kSurface.getCoordinate(aiTriIndex[0], kTri0);

        Point3f kTri1 = new Point3f();
        m_kSurface.getCoordinate(aiTriIndex[1], kTri1);

        Point3f kTri2 = new Point3f();
        m_kSurface.getCoordinate(aiTriIndex[2], kTri2);

        Vector3f kEdge = new Vector3f(kTri0.x - kTri1.x, kTri0.y - kTri1.y, kTri0.z - kTri1.z);
        kEdge.normalize();

        Vector3f kP_Edge = new Vector3f(kPoint.x - kTri1.x, kPoint.y - kTri1.y, kPoint.z - kTri1.z);
        float fDot = kEdge.dot(kP_Edge);
        Point3f kNewPoint = new Point3f((fDot * kEdge.x) + kTri1.x, (fDot * kEdge.y) + kTri1.y,
                                        (fDot * kEdge.z) + kTri1.z);

        Vector3f kNewNormal;
        TexCoord3f kNewTexCoord;
        Color4f kNewColor;

        if (kNewPoint.epsilonEquals(kPoint, m_fEpsilon)) {
            kPoint.x = kNewPoint.x;
            kPoint.y = kNewPoint.y;
            kPoint.z = kNewPoint.z;

            kNewNormal = getNormal(kMesh, aiTriIndex[0], aiTriIndex[1]);
            kNormal.x = kNewNormal.x;
            kNormal.y = kNewNormal.y;
            kNormal.z = kNewNormal.z;

            kNewTexCoord = getTexCoord(kMesh, aiTriIndex[0], aiTriIndex[1]);
            kTexCoord.x = kNewTexCoord.x;
            kTexCoord.y = kNewTexCoord.y;
            kTexCoord.z = kNewTexCoord.z;

            kNewColor = getColor(kMesh, aiTriIndex[0], aiTriIndex[1]);
            kColor.x = kNewColor.x;
            kColor.y = kNewColor.y;
            kColor.z = kNewColor.z;
            kColor.w = kNewColor.w;
            iReturn = 2;
        }

        if (iReturn == -1) {
            kEdge = new Vector3f(kTri0.x - kTri2.x, kTri0.y - kTri2.y, kTri0.z - kTri2.z);
            kEdge.normalize();
            kP_Edge = new Vector3f(kPoint.x - kTri2.x, kPoint.y - kTri2.y, kPoint.z - kTri2.z);

            fDot = kEdge.dot(kP_Edge);
            kNewPoint = new Point3f((fDot * kEdge.x) + kTri2.x, (fDot * kEdge.y) + kTri2.y, (fDot * kEdge.z) + kTri2.z);

            if (kNewPoint.epsilonEquals(kPoint, m_fEpsilon)) {
                kPoint.x = kNewPoint.x;
                kPoint.y = kNewPoint.y;
                kPoint.z = kNewPoint.z;

                kNewNormal = getNormal(kMesh, aiTriIndex[0], aiTriIndex[2]);
                kNormal.x = kNewNormal.x;
                kNormal.y = kNewNormal.y;
                kNormal.z = kNewNormal.z;

                kNewTexCoord = getTexCoord(kMesh, aiTriIndex[0], aiTriIndex[2]);
                kTexCoord.x = kNewTexCoord.x;
                kTexCoord.y = kNewTexCoord.y;
                kTexCoord.z = kNewTexCoord.z;

                kNewColor = getColor(kMesh, aiTriIndex[0], aiTriIndex[2]);
                kColor.x = kNewColor.x;
                kColor.y = kNewColor.y;
                kColor.z = kNewColor.z;
                kColor.w = kNewColor.w;
                iReturn = 1;
            }
        }

        if (iReturn == -1) {
            kEdge = new Vector3f(kTri2.x - kTri1.x, kTri2.y - kTri1.y, kTri2.z - kTri1.z);
            kEdge.normalize();
            kP_Edge = new Vector3f(kPoint.x - kTri1.x, kPoint.y - kTri1.y, kPoint.z - kTri1.z);
            fDot = kEdge.dot(kP_Edge);
            kNewPoint = new Point3f((fDot * kEdge.x) + kTri1.x, (fDot * kEdge.y) + kTri1.y, (fDot * kEdge.z) + kTri1.z);

            if (kNewPoint.epsilonEquals(kPoint, m_fEpsilon)) {
                kPoint.x = kNewPoint.x;
                kPoint.y = kNewPoint.y;
                kPoint.z = kNewPoint.z;

                kNewNormal = getNormal(kMesh, aiTriIndex[2], aiTriIndex[1]);
                kNormal.x = kNewNormal.x;
                kNormal.y = kNewNormal.y;
                kNormal.z = kNewNormal.z;

                kNewTexCoord = getTexCoord(kMesh, aiTriIndex[2], aiTriIndex[1]);
                kTexCoord.x = kNewTexCoord.x;
                kTexCoord.y = kNewTexCoord.y;
                kTexCoord.z = kNewTexCoord.z;

                kNewColor = getColor(kMesh, aiTriIndex[2], aiTriIndex[1]);
                kColor.x = kNewColor.x;
                kColor.y = kNewColor.y;
                kColor.z = kNewColor.z;
                kColor.w = kNewColor.w;
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
            kNormal.x = kNewNormal.x;
            kNormal.y = kNewNormal.y;
            kNormal.z = kNewNormal.z;

            kNewTexCoord = getTexCoord(kMesh, aiTriIndex);
            kTexCoord.x = kNewTexCoord.x;
            kTexCoord.y = kNewTexCoord.y;
            kTexCoord.z = kNewTexCoord.z;

            kNewColor = getColor(kMesh, aiTriIndex);
            kColor.x = kNewColor.x;
            kColor.y = kNewColor.y;
            kColor.z = kNewColor.z;
            kColor.w = kNewColor.w;
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

        if (m_akCoordinates != null) {

            for (int iVertex = 0; iVertex < m_iVertexCount; iVertex++) {
                m_akCoordinates[iVertex] = null;
                m_akEdgeList[iVertex].clear();
                m_akEdgeList[iVertex] = null;
            }

            m_akEdgeList = null;
            m_akCoordinates = null;
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
    private boolean contains(LinkedList kTriList, Point3i kNewTri) {
        boolean bReturn = false;
        Point3i kTri;

        for (int iNewTri = 0; iNewTri < kTriList.size(); iNewTri++) {
            kTri = (Point3i) m_kNewTriangles.get(iNewTri);

            if (triangleEquals(kNewTri.x, kNewTri.y, kNewTri.z, kTri.x, kTri.y, kTri.z)) {
                bReturn = true;
            }
        }

        return bReturn;
    }

    /**
     * Create the edges list from the given surface triangle mesh.
     *
     * @param   kMesh  ModelTriangleMesh surface
     *
     * @return  boolean success or not
     */
    private boolean createEdgeLists(ModelTriangleMesh kMesh) {

        if (m_akCoordinates != null) {
            for (int iVert = 0; iVert < m_akCoordinates.length; iVert++) {
                m_akCoordinates[iVert] = null;
            }
        }
        m_akCoordinates = null;

        if (m_akNormals != null) {
            for (int iVert = 0; iVert < m_akNormals.length; iVert++) {
                m_akNormals[iVert] = null;
            }
        }
        m_akNormals = null;

        if (m_akTexCoords != null) {
            for (int iVert = 0; iVert < m_akTexCoords.length; iVert++) {
                m_akTexCoords[iVert] = null;
            }
        }
        m_akTexCoords = null;

        if (m_akColors != null) {
            for (int iVert = 0; iVert < m_akColors.length; iVert++) {
                m_akColors[iVert] = null;
            }
        }
        m_akColors = null;


        if (m_akEdgeList != null) {
            for (int iEdge = 0; iEdge < m_akEdgeList.length; iEdge++) {
                m_akEdgeList[iEdge].clear();
            }
        }

        m_akEdgeList = null;

        if (m_aiIndex != null) {
            m_aiIndex = null;
        }

        m_iVertexCount = kMesh.getVertexCount();
        m_akCoordinates = new Point3f[m_iVertexCount];
        m_akNormals = new Vector3f[m_iVertexCount];
        m_akTexCoords = new TexCoord3f[m_iVertexCount];
        m_akColors = new Color4f[m_iVertexCount];

        m_akEdgeList = new LinkedList[m_iVertexCount];

        for (int iEdge = 0; iEdge < m_iVertexCount; iEdge++) {
            m_akEdgeList[iEdge] = new LinkedList();
            m_akCoordinates[iEdge] = new Point3f();
            m_akNormals[iEdge] = new Vector3f();
            m_akTexCoords[iEdge] = new TexCoord3f();
            m_akColors[iEdge] = new Color4f( );
        }

        kMesh.getCoordinates(0, m_akCoordinates);
        kMesh.getNormals(0, m_akNormals);
        kMesh.getTextureCoordinates(0, 0, m_akTexCoords);
        kMesh.getColors(0, m_akColors);

        m_iIndexCount = kMesh.getIndexCount();
        m_aiIndex = new int[m_iIndexCount];
        kMesh.getCoordinateIndices(0, m_aiIndex);

        int iNumTris = m_iIndexCount / 3;

        LinkedList kEndPoints = new LinkedList();
        boolean bReturn = true;

        for (int iTri = 0; iTri < iNumTris; iTri++) {
            int i0 = m_aiIndex[(iTri * 3) + 0];
            int i1 = m_aiIndex[(iTri * 3) + 1];
            int i2 = m_aiIndex[(iTri * 3) + 2];

            addEdge(i0, i1);
            addEdge(i0, i2);
            addEdge(i1, i2);

            if (i0 != i1) {
                findTriPoints(i0, i1, kEndPoints);

                if ((kEndPoints.size() > 2) || (kEndPoints.size() == 0)) {
                    //                     drawGeodesicPoint( m_akCoordinates[ i0 ], new Color3f( 1, 1, 1 ) );
                    //                     drawGeodesicPoint( m_akCoordinates[ i1 ], new Color3f( 1, 1, 1 ) );

                    //                     System.err.println( "CEL: iTri: " + iTri + " " + i0 + " " + i1 );
                    //                     for ( int iEnd = 0; iEnd < kEndPoints.size(); iEnd++ )
                    //                     {
                    //                         System.err.println( "   iEnd: " + ((Integer)kEndPoints.get( iEnd )).intValue() );
                    //                         drawGeodesicPoint( m_akCoordinates[ ((Integer)kEndPoints.get( iEnd )).intValue() ], new Color3f( 1, 0, 1 ) );
                    //                     }

                    bReturn = false;
                }
            }

            if (i0 != i2) {
                findTriPoints(i0, i2, kEndPoints);

                if ((kEndPoints.size() > 2) || (kEndPoints.size() == 0)) {
                    //                     drawGeodesicPoint( m_akCoordinates[ i0 ], new Color3f( 1, 1, 1 ) );
                    //                     drawGeodesicPoint( m_akCoordinates[ i2 ], new Color3f( 1, 1, 1 ) );


                    //                     System.err.println( "CEL: iTri: " + iTri + " " +  i0 + " " + i2 );
                    //                     for ( int iEnd = 0; iEnd < kEndPoints.size(); iEnd++ )
                    //                     {
                    //                         System.err.println( "   iEnd: " + ((Integer)kEndPoints.get( iEnd )).intValue() );
                    //                         drawGeodesicPoint( m_akCoordinates[ ((Integer)kEndPoints.get( iEnd )).intValue() ], new Color3f( 1, 0, 1 ) );
                    //                     }
                    bReturn = false;
                }
            }

            if (i1 != i2) {
                findTriPoints(i1, i2, kEndPoints);

                if ((kEndPoints.size() > 2) || (kEndPoints.size() == 0)) {
                    //                     drawGeodesicPoint( m_akCoordinates[ i1 ], new Color3f( 1, 1, 1 ) );
                    //                     drawGeodesicPoint( m_akCoordinates[ i2 ], new Color3f( 1, 1, 1 ) );

                    //                     System.err.println( "CEL: iTri: " + iTri + " " + i1 + " " + i2 );
                    //                     for ( int iEnd = 0; iEnd < kEndPoints.size(); iEnd++ )
                    //                     {
                    //                         System.err.println( "   iEnd: " + ((Integer)kEndPoints.get( iEnd )).intValue() );
                    //                         drawGeodesicPoint( m_akCoordinates[ ((Integer)kEndPoints.get( iEnd )).intValue() ], new Color3f( 1, 0, 1 ) );
                    //                     }
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
     * @param   kMesh            ModelTriangleMesh surface
     * @param   iVertexCount     int new vertex count
     * @param   iOldVertexCount  int old vertex cunt
     *
     * @return  ModelTriangleMesh surface
     */
    private ModelTriangleMesh createNewMesh(ModelTriangleMesh kMesh, int iVertexCount, int iOldVertexCount) {

        /* Add new vertices and new normals to the mesh *BEFORE* adding new
         * triangles, that way we can correct the triangle index order for the
         * right front-facing normals : */
        Point3f[] akVertices = new Point3f[iVertexCount];
        Vector3f[] akNormals = new Vector3f[iVertexCount];
        TexCoord3f[] akTexCoords = new TexCoord3f[iVertexCount];
        Color4f[] akColors = new Color4f[iVertexCount];

        int iIndex = 0;

        for (int iVert = 0; iVert < kMesh.getVertexCount(); iVert++) {
            Point3f kCoordinate = new Point3f();
            kMesh.getCoordinate(iVert, kCoordinate);

            Vector3f kNormal = new Vector3f();
            kMesh.getNormal(iVert, kNormal);

            TexCoord3f kTexCoord = new TexCoord3f();
            kMesh.getTextureCoordinate(0, iVert, kTexCoord);

            Color4f kColor = new Color4f( );
            kMesh.getColor(iVert, kColor);

            boolean bAdd = true;

            if (m_aiIndexShift != null) {

                if (m_aiIndexShift[iVert] == -1) {
                    bAdd = false;
                }
            }

            if (bAdd && (iIndex < iVertexCount)) {
                akVertices[iIndex] = new Point3f(kCoordinate);
                akNormals[iIndex] = new Vector3f(kNormal);
                akTexCoords[iIndex] = new TexCoord3f(kTexCoord);
                akColors[iIndex] = new Color4f(kColor);
                iIndex++;
            }
        }

        for (int iNewIndex = 0; iNewIndex < m_kNewVerts.size(); iNewIndex++) {
            akVertices[iIndex] = new Point3f((Point3f) m_kNewVerts.get(iNewIndex));
            akNormals[iIndex] = new Vector3f((Vector3f) m_kNewNormals.get(iNewIndex));
            akTexCoords[iIndex] = new TexCoord3f((TexCoord3f) m_kNewTexCoords.get(iNewIndex));
            akColors[iIndex] = new Color4f((Color4f) m_kNewColors.get(iNewIndex));
            iIndex++;
        }

        /* Get the old triangles: */
        int iIndexCount = kMesh.getIndexCount();
        int[] aiIndex = new int[iIndexCount];
        kMesh.getCoordinateIndices(0, aiIndex);

        int iNumTris = iIndexCount / 3;

        /* remove m_kRemoveTriangles from the mesh: */
        LinkedList kNewConnect = new LinkedList();
        Point3i aiRemoveTri = new Point3i();
        int iNumAdded = 0;
        int iNumRemoved = 0;

        for (int iTri = 0; iTri < iNumTris; iTri++) {
            boolean bAdd = true;

            if (m_abRemoveTris != null) {
                bAdd = !m_abRemoveTris[iTri];
            } else {

                for (int iRemoveTri = 0; iRemoveTri < m_kRemoveTriangles.size(); iRemoveTri++) {
                    aiRemoveTri = (Point3i) m_kRemoveTriangles.get(iRemoveTri);

                    if (triangleEquals(aiIndex[(iTri * 3) + 0], aiIndex[(iTri * 3) + 1], aiIndex[(iTri * 3) + 2],
                                           aiRemoveTri.x, aiRemoveTri.y, aiRemoveTri.z)) {
                        bAdd = false;

                        break;
                    }
                }
            }

            if (bAdd) {

                if ((iVertexCount < iOldVertexCount) && (m_aiIndexShift != null)) {
                    int i0 = aiIndex[(iTri * 3) + 0];
                    int i1 = aiIndex[(iTri * 3) + 1];
                    int i2 = aiIndex[(iTri * 3) + 2];
                    i0 -= m_aiIndexShift[i0];
                    i1 -= m_aiIndexShift[i1];
                    i2 -= m_aiIndexShift[i2];
                    kNewConnect.add(iNumAdded++, new Point3i(i0, i1, i2));
                } else {
                    kNewConnect.add(iNumAdded++,
                                    new Point3i(aiIndex[(iTri * 3) + 0], aiIndex[(iTri * 3) + 1],
                                                aiIndex[(iTri * 3) + 2]));
                }
            } else {
                iNumRemoved++;
            }
        }

        /* Add new triangles to the mesh: */
        Point3i aiAddTri;

        for (int iAddTri = 0; iAddTri < m_kNewTriangles.size(); iAddTri++) {
            aiAddTri = (Point3i) m_kNewTriangles.get(iAddTri);
            sortTriIndex(aiAddTri, akVertices, akNormals);
            kNewConnect.add(iNumAdded++, new Point3i(aiAddTri));
        }

        int[] aiConnect = new int[kNewConnect.size() * 3];

        for (int iTri = 0; iTri < kNewConnect.size(); iTri++) {
            aiAddTri = (Point3i) kNewConnect.get(iTri);
            aiConnect[(iTri * 3) + 0] = aiAddTri.x;
            aiConnect[(iTri * 3) + 1] = aiAddTri.y;
            aiConnect[(iTri * 3) + 2] = aiAddTri.z;
        }


        /* Delete local variables: */
        kNewConnect.clear();
        kNewConnect = null;
        aiIndex = null;

        /* Create new mesh: */
        if ((akVertices.length == 0) || (akNormals.length == 0) || (aiConnect.length == 0)) {
            return null;
        }

        ModelTriangleMesh kCutMesh = new ModelTriangleMesh(akVertices, akNormals, akColors, akTexCoords, aiConnect);

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
    private float distance(Point3f kPoint1, Point3f kPoint2) {
        return (float) Math.sqrt(((kPoint1.x - kPoint2.x) * (kPoint1.x - kPoint2.x)) +
                                 ((kPoint1.y - kPoint2.y) * (kPoint1.y - kPoint2.y)) +
                                 ((kPoint1.z - kPoint2.z) * (kPoint1.z - kPoint2.z)));
    }

    /**
     * Draw the user-selected point as a sphere on the triangle mesh, the sphere is added to all three drawing groups:
     * m_kSmoothedGeodesicGroup, m_kDijkstraGeodesicGroup, and m_kEuclidianGeodesicGroup.
     *
     * @param  kStart  Point3f starting point
     * @param  kColor  Color3f ending point
     */
    private void drawDijkstraEuclidianPoint(Point3f kStart, Color3f kColor) {

        /* Sphere to mark the point being drawn: */
        Shape3D kSphere = new Sphere(m_fRadius).getShape();
        kSphere.getAppearance().getMaterial().setDiffuseColor(kColor);
        kSphere.setPickable(false);

        m_bGroupAdded = true;

        /* Transforms and BranchGroup to contain the sphere shape: */
        Transform3D kTransform = new Transform3D();
        kTransform.set(new Vector3f(kStart));

        TransformGroup kTransformGroup = new TransformGroup();
        kTransformGroup.setTransform(kTransform);
        kTransformGroup.addChild(kSphere.cloneTree());

        BranchGroup kBranchGroup = new BranchGroup();
        kBranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
        kBranchGroup.addChild(kTransformGroup);
        kBranchGroup.compile();

        /* Add to the Smoothed line: */
        m_kDijkstraGeodesicGroup.addChild(kBranchGroup);

        kTransform = new Transform3D();
        kTransform.set(new Vector3f(kStart));
        kTransformGroup = new TransformGroup();
        kTransformGroup.setTransform(kTransform);
        kTransformGroup.addChild(kSphere.cloneTree());
        kBranchGroup = new BranchGroup();
        kBranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
        kBranchGroup.addChild(kTransformGroup);
        kBranchGroup.compile();

        /* Add to the Euclidian line: */
        m_kEuclidianGeodesicGroup.addChild(kBranchGroup);
    }

    /**
     * Draw the user-selected point as a sphere on the triangle mesh, the sphere is added to all three drawing groups:
     * m_kSmoothedGeodesicGroup, m_kDijkstraGeodesicGroup, and m_kEuclidianGeodesicGroup.
     *
     * @param  kStart  Point3f starting point
     * @param  kColor  Color3f ending point
     */
    private void drawGeodesicPoint(Point3f kStart, Color3f kColor) {

        /* Sphere to mark the point being drawn: */
        Shape3D kSphere = new Sphere(m_fRadius).getShape();
        kSphere.getAppearance().getMaterial().setDiffuseColor(kColor);
        kSphere.setPickable(false);

        /* Transforms and BranchGroup to contain the sphere shape: */
        Transform3D kTransform = new Transform3D();
        kTransform.set(new Vector3f(kStart));

        TransformGroup kTransformGroup = new TransformGroup();
        kTransformGroup.setTransform(kTransform);
        kTransformGroup.addChild(kSphere.cloneTree());

        BranchGroup kBranchGroup = new BranchGroup();
        kBranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
        kBranchGroup.addChild(kTransformGroup);
        kBranchGroup.compile();

        /* Add to the Smoothed line: */
        m_kSmoothedGeodesicGroup.addChild(kBranchGroup);
        m_bGroupAdded = true;
    }

    /**
     * Draw the user-selected point as a sphere on the triangle mesh, the sphere is added to all three drawing groups:
     * m_kSmoothedGeodesicGroup, m_kDijkstraGeodesicGroup, and m_kEuclidianGeodesicGroup.
     *
     * @param  kStart  Point3f point coordinate
     * @param  kColor  Color3f point color
     */
    private void drawPoint(Point3f kStart, Color3f kColor) {
        drawGeodesicPoint(kStart, kColor);
        drawDijkstraEuclidianPoint(kStart, kColor);
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
        int iNumNewVerts = 0;
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

        for (int iTri = 0; iTri < iNumTris; iTri++) {
            int i0 = m_aiIndex[(iTri * 3) + 0];
            int i1 = m_aiIndex[(iTri * 3) + 1];
            int i2 = m_aiIndex[(iTri * 3) + 2];

            if ((m_aiIndexShift[i0] == -1) && (m_aiIndexShift[i1] == -1) && (m_aiIndexShift[i2] == -1)) {
                m_abRemoveTris[iTri] = true;
            } else {
                m_abRemoveTris[iTri] = false;
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
            int iNextIndex = ((Integer) m_akEdgeList[iIndex].get(iEdge)).intValue();

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
    private float findMin(Point3f kStart, int iMiddle, int iSide, Point3f kEnd, Point4f kNewPoint4) {

        if ((iSide < 0) || (iSide >= m_iVertexCount) || (iMiddle < 0) || (iMiddle >= m_iVertexCount)) {
            System.err.println("findMin: error");
        }

        Point3f kMiddle = m_akCoordinates[iMiddle];
        Point3f kSide = m_akCoordinates[iSide];


        /* Setup the vector from Side to Middle, we will add a fraction of
         * this vector to Middle each time through the loop, creating a new
         * point and re-evaluate the new path. */
        Vector3f kDiff = new Vector3f();
        kDiff.x = kSide.x - kMiddle.x;
        kDiff.y = kSide.y - kMiddle.y;
        kDiff.z = kSide.z - kMiddle.z;

        kDiff.x /= 10.0;
        kDiff.y /= 10.0;
        kDiff.z /= 10.0;

        /* fDistance is used the path length of the current path
         * Start-newpoint-End: */
        float fDistance;

        /* The first path is Start-Middle-End, find that distance: */
        Vector3f kPath = new Vector3f();
        kPath.x = kStart.x - kMiddle.x;
        kPath.y = kStart.y - kMiddle.y;
        kPath.z = kStart.z - kMiddle.z;

        fDistance = kPath.length();
        kPath.x = kMiddle.x - kEnd.x;
        kPath.y = kMiddle.y - kEnd.y;
        kPath.z = kMiddle.z - kEnd.z;

        fDistance += kPath.length();

        float fMin1 = fDistance;
        Point3f kNewPoint1 = new Point3f(kMiddle);
        Point3f kNewPoint = new Point3f(kMiddle);

        /* Loop, adding 1/10 of the vector between Middle-Side to Middle and
         * reevaluate the path Start-newpoint-End, keep track of the point
         * that is the minimum path length: */
        int iMin = -1;

        for (int j = 0; j < 10; j++) {
            kNewPoint1.x += kDiff.x;
            kNewPoint1.y += kDiff.y;
            kNewPoint1.z += kDiff.z;

            kPath.x = kStart.x - kNewPoint1.x;
            kPath.y = kStart.y - kNewPoint1.y;
            kPath.z = kStart.z - kNewPoint1.z;

            fDistance = kPath.length();

            kPath.x = kNewPoint1.x - kEnd.x;
            kPath.y = kNewPoint1.y - kEnd.y;
            kPath.z = kNewPoint1.z - kEnd.z;

            fDistance += kPath.length();

            if (fDistance < fMin1) {
                iMin = j;
                fMin1 = fDistance;
                kNewPoint.x = kNewPoint1.x;
                kNewPoint.y = kNewPoint1.y;
                kNewPoint.z = kNewPoint1.z;
            }
        }

        if (iMin == -1) {
            kNewPoint4.x = kMiddle.x;
            kNewPoint4.y = kMiddle.y;
            kNewPoint4.z = kMiddle.z;
            kNewPoint4.w = iMiddle;
        } else if (iMin == 9) {
            kNewPoint4.x = kSide.x;
            kNewPoint4.y = kSide.y;
            kNewPoint4.z = kSide.z;
            kNewPoint4.w = iSide;
        } else {
            kNewPoint4.x = kNewPoint.x;
            kNewPoint4.y = kNewPoint.y;
            kNewPoint4.z = kNewPoint.z;
            kNewPoint4.w = -1;
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
     * @param   kSourceMesh             ModelTriangleMesh surface mesh
     * @param   kGeodesic_Closed_Loops  LinkedList closed path
     *
     * @return  int
     */
    private int findNewMeshes(ModelTriangleMesh kSourceMesh, LinkedList kGeodesic_Closed_Loops) {
        LinkedList kClosedLoop = (LinkedList) kGeodesic_Closed_Loops.get(0);
        int iNumDeletedVerts = extractNewMesh(kClosedLoop);

        if (iNumDeletedVerts < m_iVertexCount) {
            outputDeletedAsNew();
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
        int iSmallest = ((Integer) m_kBorder.get(0)).intValue();

        /* Loop over all the vertices in the border: */
        for (int iBorder = 0; iBorder < iSize; iBorder++) {
            iVertex = ((Integer) m_kBorder.get(iBorder)).intValue();

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
    private int findTriPoints(int iNode1, int iNode2, LinkedList kEndPoints) {
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

                    if (((Integer) kEndPoints.get(iEnd)).intValue() == i2) {
                        bAdd = false;
                    }
                }

                if (bAdd) {
                    kEndPoints.add(new Integer(i2));
                }

            } else if (((i0 == iNode1) && (i2 == iNode2)) || ((i0 == iNode2) && (i2 == iNode1))) {

                for (int iEnd = 0; iEnd < kEndPoints.size(); iEnd++) {

                    if (((Integer) kEndPoints.get(iEnd)).intValue() == i1) {
                        bAdd = false;
                    }
                }

                if (bAdd) {
                    kEndPoints.add(new Integer(i1));
                }
            } else if (((i1 == iNode1) && (i2 == iNode2)) || ((i1 == iNode2) && (i2 == iNode1))) {

                for (int iEnd = 0; iEnd < kEndPoints.size(); iEnd++) {

                    if (((Integer) kEndPoints.get(iEnd)).intValue() == i0) {
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
        LinkedList kGeodesic;

        if (m_iNumWorking > 1) {
            kGeodesic = new LinkedList();

            /* The total number of points in the Geodesic and Dijkstra's path,
             * for all segments: */
            int iGeodesicCount = 0;

            /* For each segment in the working paths: */
            for (int iList = 0; iList < m_iNumWorking; iList++) {

                /* Get the Geodesic segment: */
                LinkedList kGeodesicSegment = (LinkedList) m_kGeodesic_Working.get(iList);

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
            kGeodesic = (LinkedList) m_kGeodesic_Working.get(0);
        }

        /* Last vert in the last list: */
        /* If it's a closed loop: */
        if (kGeodesic.getFirst().equals(kGeodesic.getLast())) {
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
     * Calculates and returns the start point normal for a new starting point inside an existing triangle. The new
     * normal is the average of the normals at each point in the triangle the starting point is inside
     *
     * @param   kMesh    ModelTriangleMesh surface mesh
     * @param   aiIndex  int[] 3 triangle points
     *
     * @return  Vector3f Average normal of the triangle.
     */
    private Vector3f getNormal(ModelTriangleMesh kMesh, int[] aiIndex) {
        Vector3f kNormal = new Vector3f();
        Vector3f kNormal0 = new Vector3f();
        kMesh.getNormal(aiIndex[0], kNormal0);

        Vector3f kNormal1 = new Vector3f();
        kMesh.getNormal(aiIndex[1], kNormal1);

        Vector3f kNormal2 = new Vector3f();
        kMesh.getNormal(aiIndex[2], kNormal2);

        kNormal.x = (kNormal0.x + kNormal1.x + kNormal2.x) / 3.0f;

        kNormal.y = (kNormal0.y + kNormal1.y + kNormal2.y) / 3.0f;

        kNormal.z = (kNormal0.z + kNormal1.z + kNormal2.z) / 3.0f;


        kNormal.normalize();

        return kNormal;
    }

    /**
     * Calculates and returns the start point texture coordinate for a new
     * starting point inside an existing triangle. The new texture coordinate
     * is the average of the texture coordinates at each point in the triangle
     * the starting point is inside
     *
     * @param   kMesh    ModelTriangleMesh surface mesh
     * @param   aiIndex  int[] 3 triangle points
     *
     * @return  Texture3f Average texture coordinate of the triangle.
     */
    private TexCoord3f getTexCoord(ModelTriangleMesh kMesh, int[] aiIndex) {
        TexCoord3f kTexCoord = new TexCoord3f();
        TexCoord3f kTexCoord0 = new TexCoord3f();
        kMesh.getTextureCoordinate(0, aiIndex[0], kTexCoord0);

        TexCoord3f kTexCoord1 = new TexCoord3f();
        kMesh.getTextureCoordinate(0, aiIndex[1], kTexCoord1);

        TexCoord3f kTexCoord2 = new TexCoord3f();
        kMesh.getTextureCoordinate(0, aiIndex[2], kTexCoord2);

        kTexCoord.x = (kTexCoord0.x + kTexCoord1.x + kTexCoord2.x) / 3.0f;

        kTexCoord.y = (kTexCoord0.y + kTexCoord1.y + kTexCoord2.y) / 3.0f;

        kTexCoord.z = (kTexCoord0.z + kTexCoord1.z + kTexCoord2.z) / 3.0f;

        return kTexCoord;
    }

    /**
     * Calculates and returns the start point color for a new starting point
     * inside an existing triangle. The new color is the average of the colors
     * at each point in the triangle the starting point is inside
     *
     * @param   kMesh    ModelTriangleMesh surface mesh
     * @param   aiIndex  int[] 3 triangle points
     *
     * @return  Color4f Average color of the triangle.
     */
    private Color4f getColor(ModelTriangleMesh kMesh, int[] aiIndex) {
        Color4f kColor = new Color4f();
        Color4f kColor0 = new Color4f();
        kMesh.getColor(aiIndex[0], kColor0);

        Color4f kColor1 = new Color4f();
        kMesh.getColor(aiIndex[1], kColor1);

        Color4f kColor2 = new Color4f();
        kMesh.getColor(aiIndex[2], kColor2);

        kColor.x = (kColor0.x + kColor1.x + kColor2.x) / 3.0f;
        kColor.y = (kColor0.y + kColor1.y + kColor2.y) / 3.0f;
        kColor.z = (kColor0.z + kColor1.z + kColor2.z) / 3.0f;
        kColor.w = (kColor0.w + kColor1.w + kColor2.w) / 3.0f;

        return kColor;
    }


    /**
     * Get the the triangle normal from the given triangle index.
     *
     * @param   kMesh    ModelTriangleMesh surface mesh
     * @param   iIndex1  int triangle point index 1
     * @param   iIndex2  int triangle point index 2
     *
     * @return  Vector3f normal of the triangle
     */
    private Vector3f getNormal(ModelTriangleMesh kMesh, int iIndex1, int iIndex2) {
        Vector3f kSide1 = new Vector3f();
        Vector3f kSide2 = new Vector3f();
        kMesh.getNormal(iIndex1, kSide1);
        kMesh.getNormal(iIndex2, kSide2);

        Vector3f kNormal = new Vector3f();
        kNormal.x = (kSide1.x + kSide2.x) / 2.0f;
        kNormal.y = (kSide1.y + kSide2.y) / 2.0f;
        kNormal.z = (kSide1.z + kSide2.z) / 2.0f;

        kNormal.normalize();

        kSide1 = null;
        kSide2 = null;

        return kNormal;
    }

    /**
     * Get the the triangle texture coordinate from the given triangle index.
     *
     * @param   kMesh    ModelTriangleMesh surface mesh
     * @param   iIndex1  int triangle point index 1
     * @param   iIndex2  int triangle point index 2
     *
     * @return  interpolated texture coordinate the triangle
     */
    private TexCoord3f getTexCoord(ModelTriangleMesh kMesh, int iIndex1, int iIndex2) {
        TexCoord3f kSide1 = new TexCoord3f();
        TexCoord3f kSide2 = new TexCoord3f();
        kMesh.getTextureCoordinate(0, iIndex1, kSide1);
        kMesh.getTextureCoordinate(0, iIndex2, kSide2);

        TexCoord3f kTexCoord = new TexCoord3f();
        kTexCoord.x = (kSide1.x + kSide2.x) / 2.0f;
        kTexCoord.y = (kSide1.y + kSide2.y) / 2.0f;
        kTexCoord.z = (kSide1.z + kSide2.z) / 2.0f;

        kSide1 = null;
        kSide2 = null;

        return kTexCoord;
    }

    /**
     * Get the the triangle color from the given triangle index.
     *
     * @param   kMesh    ModelTriangleMesh surface mesh
     * @param   iIndex1  int triangle point index 1
     * @param   iIndex2  int triangle point index 2
     *
     * @return  interpolated color the triangle
     */
    private Color4f getColor(ModelTriangleMesh kMesh, int iIndex1, int iIndex2) {
        Color4f kSide1 = new Color4f();
        Color4f kSide2 = new Color4f();
        kMesh.getColor(iIndex1, kSide1);
        kMesh.getColor(iIndex2, kSide2);

        Color4f kColor = new Color4f();
        kColor.x = (kSide1.x + kSide2.x) / 2.0f;
        kColor.y = (kSide1.y + kSide2.y) / 2.0f;
        kColor.z = (kSide1.z + kSide2.z) / 2.0f;
        kColor.w = (kSide1.w + kSide2.w) / 2.0f;

        kSide1 = null;
        kSide2 = null;

        return kColor;
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
        int iFirst = (int) ((Point4f) kPath.get(0)).w;
        int iLast = (int) ((Point4f) kPath.get(kPath.size() - 1)).w;

        for (int iPath = 0; iPath < kPath.size(); iPath++) {

            if (iIndex == ((Point4f) kPath.get(iPath)).w) {

                if ((iIndex == iFirst) || (iIndex == iLast)) {

                    if (!bOpen) {
                        return iVertexCount + (kPath.size() - 2);
                    } else {
                        return iIndex;
                    }
                } else {
                    return iVertexCount + (iPath - 1);
                }
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
        LinkedList kStartEndList = (LinkedList) m_kStartEndList.get(iWhich);

        Point3i kEndIndex = new Point3i((Point3i) kStartEndList.get(0));
        Point3i kStartIndex = new Point3i((Point3i) kStartEndList.get(1));
        m_kEndPoint = new Point3f((Point3f) kStartEndList.get(2));
        m_kStartPoint = new Point3f((Point3f) kStartEndList.get(3));

        m_aiEndIndex[0] = kEndIndex.x;
        m_aiEndIndex[1] = kEndIndex.y;
        m_aiEndIndex[2] = kEndIndex.z;

        m_aiStartIndex[0] = kStartIndex.x;
        m_aiStartIndex[1] = kStartIndex.y;
        m_aiStartIndex[2] = kStartIndex.z;
    }

    /**
     * Initializes the colors for the first point on the curve, and the successive points.
     */
    private void initColors() {
        m_kPickColors = new Color3f[2];
        m_kPickColors[0] = new Color3f(1, 0, 0);
        m_kPickColors[1] = new Color3f(1, 1, 0);
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
    private void initializeGeodesic(float fPercentage) {

        /* Check that m_kStartPoint and m_kEndPoint are not already on the
         * mesh, otherwise they will be added and the triangles
         * re-triangulated: */
        boolean bFirstSegment = false;

        if (m_kEndPoint.equals(m_kFirstPoint)) {
            bFirstSegment = true;
        }

        m_bEndpointChanged = false;
        m_iVertexCount = m_kSurface.getVertexCount();

        Point3f kVertex = new Point3f();

        for (int iVert = 0; iVert < m_iVertexCount; iVert++) {
            m_kSurface.getCoordinate(iVert, kVertex);

            if (m_kStartPoint.epsilonEquals(kVertex, m_fEpsilon)) {
                m_kStartPoint = new Point3f(kVertex);
                m_iStart = iVert;
            }

            if (m_kEndPoint.epsilonEquals(kVertex, m_fEpsilon)) {

                if (m_kEndPoint.equals(m_kFirstPoint)) {
                    m_kFirstPoint = new Point3f(kVertex);
                }

                m_bEndpointChanged = true;
                m_kEndPoint = new Point3f(kVertex);
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
                Point3f kPoint = new Point3f();
                m_kSurface.getCoordinate(m_aiEndIndex[0], kPoint);

                float fDistance0 = distance(m_kEndPoint, kPoint);

                m_kSurface.getCoordinate(m_aiEndIndex[1], kPoint);

                float fDistance1 = distance(m_kEndPoint, kPoint);

                m_kSurface.getCoordinate(m_aiEndIndex[2], kPoint);

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
                m_kSurface.getCoordinate(m_iEnd, m_kEndPoint);

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

        TexCoord3f kStartTexCoord = new TexCoord3f();
        TexCoord3f kEndTexCoord = new TexCoord3f();

        Color4f kStartColor = new Color4f();
        Color4f kEndColor = new Color4f();
        m_fEpsilon /= 5.0f;

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

        /* Normals: */
        m_akNormals = new Vector3f[m_iVertexCount];

        /* TextureCoordinates: */
        m_akTexCoords = new TexCoord3f[m_iVertexCount];

        /* Colors: */
        m_akColors = new Color4f[m_iVertexCount];

        /* Vertices: */
        m_akCoordinates = new Point3f[m_iVertexCount];

        /* EdgeLists, one per vertex: */
        m_akEdgeList = new LinkedList[m_iVertexCount];

        /* The touched, but not relaxed vertices for Dijkstra's search: */
        m_kBorder = new LinkedList();

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
                m_akCoordinates[iVertex] = new Point3f();
                m_kSurface.getCoordinate(iVertex, m_akCoordinates[iVertex]);

                m_akNormals[iVertex] = new Vector3f();
                m_kSurface.getNormal(iVertex, m_akNormals[iVertex]);

                m_akTexCoords[iVertex] = new TexCoord3f();
                m_kSurface.getTextureCoordinate(0, iVertex, m_akTexCoords[iVertex]);

                m_akColors[iVertex] = new Color4f();
                m_kSurface.getColor(iVertex, m_akColors[iVertex]);
            }
            /* All the vertices have been added, if the number of new vertices
             * is 2, add both the start and end vertices: */
            else if (iNumberNewVertices == 2) {

                if (iVertex == (m_iVertexCount - 2)) {
                    m_akCoordinates[iVertex] = new Point3f(m_kStartPoint);
                    m_akNormals[iVertex] = new Vector3f(kStartNormal);
                    m_akTexCoords[iVertex] = new TexCoord3f(kStartTexCoord);
                    m_akColors[iVertex] = new Color4f(kStartColor);
                } else if (iVertex == (m_iVertexCount - 1)) {
                    m_akCoordinates[iVertex] = new Point3f(m_kEndPoint);
                    m_akNormals[iVertex] = new Vector3f(kEndNormal);
                    m_akTexCoords[iVertex] = new TexCoord3f(kEndTexCoord);
                    m_akColors[iVertex] = new Color4f(kEndColor);
                }
            }
            /* All the vertices have been added, if the number of new vertices
             * is 1, add based on the bAdd flag set at the beginning of this
             * function: */
            else if (iNumberNewVertices == 1) {

                if (bAddStart) {
                    m_akCoordinates[iVertex] = new Point3f(m_kStartPoint);
                    m_akNormals[iVertex] = new Vector3f(kStartNormal);
                    m_akTexCoords[iVertex] = new TexCoord3f(kStartTexCoord);
                    m_akColors[iVertex] = new Color4f(kStartColor);
                } else if (bAddEnd) {
                    m_akCoordinates[iVertex] = new Point3f(m_kEndPoint);
                    m_akNormals[iVertex] = new Vector3f(kEndNormal);
                    m_akTexCoords[iVertex] = new TexCoord3f(kEndTexCoord);
                    m_akColors[iVertex] = new Color4f(kEndColor);
                }
            }

            m_akEdgeList[iVertex] = new LinkedList();

            /* Calculate straight-line distance to end point*/
            fDistance = distance(m_kEndPoint, m_akCoordinates[iVertex]);

            /* Initialize search data variables: */
            m_fRemainingWeight[iVertex] = fPercentage * fDistance;
            m_fWeight[iVertex] = Float.MAX_VALUE;
            m_bRelaxed[iVertex] = false;
            m_iPrevious[iVertex] = -1;
        }


        /* Copy the triangles indices: */
        int iIndexCount = m_kSurface.getIndexCount();
        m_iIndexCount = iIndexCount;

        /* Add 6 * the number of new vertices in triangles: */
        if (bAddStart) {
            m_iIndexCount += 6;
        }

        if (bAddEnd) {
            m_iIndexCount += 6;
        }


        int[] aiIndex = new int[iIndexCount];
        m_aiIndex = new int[m_iIndexCount];
        m_kSurface.getCoordinateIndices(0, aiIndex);

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
        m_kModified = new ModelTriangleMesh(m_akCoordinates, m_akNormals, m_akColors, m_akTexCoords, m_aiIndex);
        aiIndex = null;

        if (bFirstSegment) {
            m_kFirstPoint = new Point3f(m_kEndPoint);
            m_aiFirstIndex[0] = m_aiEndIndex[0];
            m_aiFirstIndex[1] = m_aiEndIndex[1];
            m_aiFirstIndex[2] = m_aiEndIndex[2];
        }

    }


    /**
     * onRight determines if a given point on a triangle, kSide, is to the right of or to the left of the vector
     * specified by the vertices kPrev - kPoint.
     *
     * @param   kMesh       ModelTriangleMesh surface mesh
     * @param   iPrevIndex  int previous vertice
     * @param   iIndex      int specified vertice index
     * @param   iSideIndex  int side vertice index
     *
     * @return  boolean a point on the right or triangle or not.
     */
    private boolean onRight(ModelTriangleMesh kMesh, int iPrevIndex, int iIndex, int iSideIndex) {
        Point3f kPrev = new Point3f();
        Point3f kPoint = new Point3f();
        Point3f kSide = new Point3f();

        kMesh.getCoordinate(iPrevIndex, kPrev);
        kMesh.getCoordinate(iIndex, kPoint);
        kMesh.getCoordinate(iSideIndex, kSide);

        Vector3f kMeshNormal = new Vector3f();
        kMesh.getNormal(iIndex, kMeshNormal);

        Vector3f kSide_Point = new Vector3f();
        kSide_Point.x = kSide.x - kPoint.x;
        kSide_Point.y = kSide.y - kPoint.y;
        kSide_Point.z = kSide.z - kPoint.z;
        kSide_Point.normalize();

        Vector3f kPrev_Point = new Vector3f();
        kPrev_Point.x = kPrev.x - kPoint.x;
        kPrev_Point.y = kPrev.y - kPoint.y;
        kPrev_Point.z = kPrev.z - kPoint.z;
        kPrev_Point.normalize();

        Vector3f kCross = new Vector3f();
        kCross.cross(kPrev_Point, kSide_Point);

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
    private void outputDeletedAsNew() {
        LinkedList kVertices = new LinkedList();
        LinkedList kNormals = new LinkedList();
        LinkedList kTexCoords = new LinkedList();
        LinkedList kColors = new LinkedList();

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
                if (!kVertices.contains(m_akCoordinates[i0])) {
                    kVertices.add(m_akCoordinates[i0]);
                    kNormals.add(m_akNormals[i0]);
                    kTexCoords.add(m_akTexCoords[i0]);
                    kColors.add(m_akColors[i0]);
                }

                if (!kVertices.contains(m_akCoordinates[i1])) {
                    kVertices.add(m_akCoordinates[i1]);
                    kNormals.add(m_akNormals[i1]);
                    kTexCoords.add(m_akTexCoords[i1]);
                    kColors.add(m_akColors[i1]);
                }

                if (!kVertices.contains(m_akCoordinates[i2])) {
                    kVertices.add(m_akCoordinates[i2]);
                    kNormals.add(m_akNormals[i2]);
                    kTexCoords.add(m_akTexCoords[i2]);
                    kColors.add(m_akColors[i2]);
                }

                int iNewIndex0 = kVertices.indexOf(m_akCoordinates[i0]);
                int iNewIndex1 = kVertices.indexOf(m_akCoordinates[i1]);
                int iNewIndex2 = kVertices.indexOf(m_akCoordinates[i2]);

                /* Add new tri to the index list: */
                aiConnect[(iNumNewTris * 3) + 0] = iNewIndex0;
                aiConnect[(iNumNewTris * 3) + 1] = iNewIndex1;
                aiConnect[(iNumNewTris * 3) + 2] = iNewIndex2;
                iNumNewTris++;
            }
        }

        Point3f[] kCoordinates = new Point3f[kVertices.size()];
        Vector3f[] kNormalVectors = new Vector3f[kNormals.size()];
        TexCoord3f[] kTexCoordVectors = new TexCoord3f[kTexCoords.size()];
        Color4f[] kColorVectors = new Color4f[kColors.size()];

        for (int iVert = 0; iVert < kVertices.size(); iVert++) {
            kCoordinates[iVert] = new Point3f((Point3f) kVertices.get(iVert));
            kNormalVectors[iVert] = new Vector3f((Vector3f) kNormals.get(iVert));
            kTexCoordVectors[iVert] = new TexCoord3f((TexCoord3f) kTexCoords.get(iVert));
            kColorVectors[iVert] = new Color4f((Color4f) kColors.get(iVert));
        }

        kVertices.clear();
        kVertices = null;
        kNormals.clear();
        kNormals = null;
        kTexCoords.clear();
        kTexCoords = null;
        kColors.clear();
        kColors = null;

        ModelTriangleMesh kAddMesh = new ModelTriangleMesh(kCoordinates, kNormalVectors, kColorVectors, kTexCoordVectors, aiConnect);
        if (m_kPanel != null) {
            m_kPanel.addMesh(m_kSurface, kAddMesh, new String("Geodesic_" + m_iNumNewMeshes++ + ".sur"));
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
    private void relaxEdges(int iNode) {
        float fEdgeWeight, fEdgeSmoothedWeight;
        Point3f kNode = m_akCoordinates[iNode];
        int iNeighbor;
        Integer kNeighbor;
        int iPreviousSave;
        float fPathLength, fNewPathLength;

        /* Get the number of edges that are connected to the input vertex: */
        int iNumEdges = m_akEdgeList[iNode].size();

        /* Loop over all edges: */
        for (int iEdge = 0; iEdge < iNumEdges; iEdge++) {

            /* iNeighbor is the index of the vertex connected by the current
             * edge: */
            iNeighbor = ((Integer) m_akEdgeList[iNode].get(iEdge)).intValue();

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

            fPathLength = m_fWeight[iNode] + distance(m_akCoordinates[iNode], m_akCoordinates[iNeighbor]);


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
        m_kSmoothedGeodesicGroup.removeAllChildren();
        m_kDijkstraGeodesicGroup.removeAllChildren();
        m_kEuclidianGeodesicGroup.removeAllChildren();

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
        Point3i kEndIndex = new Point3i(m_aiEndIndex[0], m_aiEndIndex[1], m_aiEndIndex[2]);

        Point3i kStartIndex = new Point3i(m_aiStartIndex[0], m_aiStartIndex[1], m_aiStartIndex[2]);
        LinkedList kStartEndList = new LinkedList();
        kStartEndList.add(0, new Point3i(kEndIndex));
        kStartEndList.add(1, new Point3i(kStartIndex));
        kStartEndList.add(2, new Point3f(m_kEndPoint));
        kStartEndList.add(3, new Point3f(m_kStartPoint));

        if (m_kStartEndList == null) {
            m_kStartEndList = new LinkedList();
        }

        m_kStartEndList.add(kStartEndList);
    }

    /**
     * The start and end surfaces ensure that the points on the geodesic curve all fall on one mesh, and that the
     * algorithm isn't trying to find a path between two unconnected meshes.
     *
     * @param  kMesh  ModelTriangleMesh surface mesh
     */
    private void setEndSurface(ModelTriangleMesh kMesh) {
        m_kEndSurface = kMesh;

        if (m_kEndSurface == m_kStartSurface) {
            setSurface(kMesh);
        }
    }


    /**
     * Determines an appropriate epsilon, based on the size of the triangles in the mesh. The epsilon is used to
     * determine if points are "close enough" to the triangle vertices or edges.
     */
    private void setEpsilon() {
        m_fEpsilon = 0;

        int iIndexCount = m_kStartSurface.getIndexCount();
        int[] aiIndex = new int[iIndexCount];
        m_kStartSurface.getCoordinateIndices(0, aiIndex);

        Point3f kPoint0 = new Point3f();
        Point3f kPoint1 = new Point3f();
        Point3f kPoint2 = new Point3f();
        int iNumTris = iIndexCount / 3;

        for (int iTri = 0; iTri < iNumTris; iTri++) {
            m_kStartSurface.getCoordinate(aiIndex[(iTri * 3) + 0], kPoint0);
            m_kStartSurface.getCoordinate(aiIndex[(iTri * 3) + 1], kPoint1);
            m_kStartSurface.getCoordinate(aiIndex[(iTri * 3) + 2], kPoint2);

            m_fEpsilon = distance(kPoint0, kPoint1) / 10.0f;

            if (m_fEpsilon > 0) {
                break;
            }

            m_fEpsilon = distance(kPoint0, kPoint2) / 10.0f;

            if (m_fEpsilon > 0) {
                break;
            }

            m_fEpsilon = distance(kPoint1, kPoint2) / 10.0f;

            if (m_fEpsilon > 0) {
                break;
            }
        }

        aiIndex = null;
        kPoint0 = null;
        kPoint1 = null;
        kPoint2 = null;
    }


    /**
     * Sets the previous start point of the geodesic curve on the mesh. Used when the last point is deleted, the start
     * point reverts to the previous start point. The point coordinates must be in local mesh coordinates.
     *
     * @param  kPoint  the point on the triangle mesh where the geodesic curve is to start, in mesh coordinates.
     */
    private void setPreviousStartPoint(Point3f kPoint) {

        if (m_kPreviousStartPoint != null) {
            m_kPreviousStartPoint = null;
        }

        m_kPreviousStartPoint = new Point3f(kPoint);
    }

    /**
     * The start and end surfaces ensure that the points on the geodesic curve all fall on one mesh, and that the
     * algorithm isn't trying to find a path between two unconnected meshes.
     *
     * @param  kMesh  surface mesh
     */
    private void setStartSurface(ModelTriangleMesh kMesh) {
        m_kStartSurface = kMesh;
        setEpsilon();
    }

    /**
     * Access function to set the triangle mesh that the geodesic curve is calculated on.
     *
     * @param  kMesh  DOCUMENT ME!
     */
    private void setSurface(ModelTriangleMesh kMesh) {
        m_kSurface = kMesh;

        if (m_kOriginal == null) {
            m_kOriginal = new ModelTriangleMesh(kMesh);
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
    private int smoothPath(int iNode, LinkedList kLeft, LinkedList kMiddle, LinkedList kRight, LinkedList kLeftTemp,
                           LinkedList kRightTemp, LinkedList kNewVertTemp) {
        Point3f kStart = m_akCoordinates[((Integer) kMiddle.get(iNode - 1)).intValue()];

        int iMiddle = ((Integer) kMiddle.get(iNode)).intValue();
        int iEnd = iNode + 1;

        for (iEnd = iNode + 1; iEnd < kMiddle.size(); iEnd++) {

            if (((Integer) kMiddle.get(iEnd)).intValue() != iMiddle) {
                break;
            }
        }

        if (iEnd == kMiddle.size()) {
            System.err.println("error smoothPath");
            System.exit(-1);
        }

        int iNumSteps = iEnd - iNode;

        Point3f kEnd = m_akCoordinates[((Integer) kMiddle.get(iEnd)).intValue()];
        Point4f kNewPoint = new Point4f();

        boolean bUseRight = true;
        float fRightPathLength = 0;
        int iRight = ((Integer) kRight.get(iNode)).intValue();

        if (iRight != -1) {
            fRightPathLength += findMin(kStart, iMiddle, iRight, kEnd, kNewPoint);

            for (int iRightNode = iNode + 1; iRightNode < (iNode + iNumSteps); iRightNode++) {

                if (iRight == ((Integer) kRight.get(iRightNode)).intValue()) {
                    break;
                }

                iRight = ((Integer) kRight.get(iRightNode)).intValue();

                if (iRight != -1) {
                    fRightPathLength += findMin(kStart, iMiddle, iRight, kEnd, kNewPoint);
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
        int iLeft = ((Integer) kLeft.get(iNode)).intValue();

        if (iLeft != -1) {
            fLeftPathLength += findMin(kStart, iMiddle, iLeft, kEnd, kNewPoint);

            for (int iLeftNode = iNode + 1; iLeftNode < (iNode + iNumSteps); iLeftNode++) {

                if (iLeft == ((Integer) kLeft.get(iLeftNode)).intValue()) {
                    break;
                }

                iLeft = ((Integer) kLeft.get(iLeftNode)).intValue();

                if (iLeft != -1) {
                    fLeftPathLength += findMin(kStart, iMiddle, iLeft, kEnd, kNewPoint);
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

        Point3f kMiddlePoint = m_akCoordinates[iMiddle];

        if ((bUseRight && bUseLeft && (fRightPathLength <= fLeftPathLength)) || (bUseRight && !bUseLeft)) {
            iRight = ((Integer) kRight.get(iNode)).intValue();

            findMin(kStart, iMiddle, iRight, kEnd, kNewPoint);
            kLeftTemp.add(new Integer(iMiddle));
            kRightTemp.add(new Integer(iRight));
            kNewVertTemp.add(new Point4f(kNewPoint));

            for (int iRightNode = iNode + 1; iRightNode < (iNode + iNumSteps); iRightNode++) {

                if (iRight == ((Integer) kRight.get(iRightNode)).intValue()) {
                    break;
                }

                iRight = ((Integer) kRight.get(iRightNode)).intValue();

                findMin(kStart, iMiddle, iRight, kEnd, kNewPoint);
                kLeftTemp.add(new Integer(iMiddle));
                kRightTemp.add(new Integer(iRight));
                kNewVertTemp.add(new Point4f(kNewPoint));
            }

            return 0;
        } else if ((bUseLeft && bUseRight && (fLeftPathLength < fRightPathLength)) || (bUseLeft && !bUseRight)) {
            iLeft = ((Integer) kLeft.get(iNode)).intValue();

            findMin(kStart, iMiddle, iLeft, kEnd, kNewPoint);
            kLeftTemp.add(new Integer(iLeft));
            kRightTemp.add(new Integer(iMiddle));
            kNewVertTemp.add(new Point4f(kNewPoint));

            for (int iLeftNode = iNode + 1; iLeftNode < (iNode + iNumSteps); iLeftNode++) {

                if (iLeft == ((Integer) kLeft.get(iLeftNode)).intValue()) {
                    break;
                }

                iLeft = ((Integer) kLeft.get(iLeftNode)).intValue();

                findMin(kStart, iMiddle, iLeft, kEnd, kNewPoint);
                kLeftTemp.add(new Integer(iLeft));
                kRightTemp.add(new Integer(iMiddle));
                kNewVertTemp.add(new Point4f(kNewPoint));
            }

            return 1;
        }

        kLeftTemp.add(new Integer(-1));
        kRightTemp.add(new Integer(-1));
        kNewVertTemp.add(new Point4f(kMiddlePoint.x, kMiddlePoint.y, kMiddlePoint.z, iMiddle));

        return -1;
    }

    /**
     * Used when new triangles are added to the mesh, either when the mesh is triangulated along the smoothed geodesic
     * curve, or when the mesh is cut. sortTriIndex sorts the triangle indices so that the triangle is always
     * front-facing and that the normals are correct for rendering
     *
     * @param  aiAddTri    Point3i added triangle vertices
     * @param  akVertices  Point3f[] triangle vertices arrray
     * @param  akNormals   Vector3f[] triangle vertices normal array
     */
    private void sortTriIndex(Point3i aiAddTri, Point3f[] akVertices, Vector3f[] akNormals) {
        int i0 = aiAddTri.x;
        int i1 = aiAddTri.y;
        int i2 = aiAddTri.z;

        Vector3f kP1_P0 = new Vector3f();
        kP1_P0.x = akVertices[i1].x - akVertices[i0].x;
        kP1_P0.y = akVertices[i1].y - akVertices[i0].y;
        kP1_P0.z = akVertices[i1].z - akVertices[i0].z;
        kP1_P0.normalize();

        Vector3f kP2_P0 = new Vector3f();
        kP2_P0.x = akVertices[i2].x - akVertices[i0].x;
        kP2_P0.y = akVertices[i2].y - akVertices[i0].y;
        kP2_P0.z = akVertices[i2].z - akVertices[i0].z;
        kP2_P0.normalize();

        Vector3f kCross = new Vector3f();
        kCross.cross(kP1_P0, kP2_P0);

        if (kCross.dot(akNormals[i0]) < 0) {
            aiAddTri.x = i1;
            aiAddTri.y = i0;
            aiAddTri.z = i2;
        }

        kP1_P0 = null;
        kP2_P0 = null;
        kCross = null;
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
     * @param   i0   int first triangle indice 1
     * @param   i1   int first triangle indice 2
     * @param   i2   int first triangle indice 3
     * @param   iP0  int second triangle indice 1
     * @param   iP1  int second triangle indice 2
     * @param   iP2  DOCUMENT ME!
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
    private boolean triangleExists(Point3i kTri) {
        int iNumTris = m_iIndexCount / 3;

        for (int iTri = 0; iTri < iNumTris; iTri++) {
            int i0 = m_aiIndex[(iTri * 3) + 0];
            int i1 = m_aiIndex[(iTri * 3) + 1];
            int i2 = m_aiIndex[(iTri * 3) + 2];

            if (triangleEquals(i0, i1, i2, kTri.x, kTri.y, kTri.z)) {
                return true;
            }
        }

        return false;
    }

    /**
     * triangulates the mesh along a single geodesic.
     *
     * @param   kMesh         ModelTriangleMesh surface mesh
     * @param   kPath         LinkedList path link list
     * @param   kLeftPath     LinkedList left path link list
     * @param   kRightPath    LinkedList right path link list
     * @param   iVertexCount  int vertex count
     *
     * @return  int number of vertices
     */
    private int triangulate(ModelTriangleMesh kMesh, LinkedList kPath, LinkedList kLeftPath, LinkedList kRightPath,
                            int iVertexCount) {
        int iNumNewVerts = 0;

        if (kPath.size() < 2) {
            return iNumNewVerts;
        }

        Point4f kPathPoint;
        Point4f kNextPoint;
        Vector3f kNormal;
        TexCoord3f kTexCoord;
        Color4f kColor;

        /* The first point on the Geodesic Path is already in the mesh, by
         * definition: */
        kPathPoint = (Point4f) kPath.get(0);

        /* Identify which point on the mesh this is: */
        int iPathIndex = (int) kPathPoint.w;
        int iPrevPathIndex = iPathIndex;
        int iNextPathIndex;

        if (iPathIndex == -1) {
            return iNumNewVerts;
        }

        /* Triangle index to add and delete: */
        Point3i kAddTri = new Point3i();
        Point3i kDeleteTri = new Point3i();

        int[] iSideIndex = new int[2];
        int iPath = 1;

        for (iPath = 1; iPath < kPath.size(); iPath++) {
            kNextPoint = (Point4f) kPath.get(iPath);
            iNextPathIndex = (int) kNextPoint.w;

            if (iPathIndex == iNextPathIndex) {
                iPrevPathIndex = iPathIndex;

                continue;
            }

            if (iNextPathIndex != -1) {

                if (iPathIndex < iVertexCount) {

                    /* Both vertices are on the mesh already, remove/add
                     * nothing: */
                    /* Update iPathIndex: */
                    iPrevPathIndex = iPathIndex;
                    iPathIndex = iNextPathIndex;
                } else {

                    /* The current vertex is not on the mesh, the next vertex
                     * is: */
                    iSideIndex[0] = ((Integer) kLeftPath.get(iPath - 1)).intValue();
                    iSideIndex[1] = ((Integer) kRightPath.get(iPath - 1)).intValue();

                    /* Remove the triangle containing the side indexes and the
                     * next vertex: */
                    kDeleteTri.x = iSideIndex[0];
                    kDeleteTri.y = iSideIndex[1];
                    kDeleteTri.z = iNextPathIndex;

                    if (triangleExists(kDeleteTri)) {
                        m_kRemoveTriangles.add(new Point3i(kDeleteTri));

                        /* Add two new triangles and quit: */
                        kAddTri.x = iSideIndex[0];
                        kAddTri.y = iPathIndex;
                        kAddTri.z = iNextPathIndex;
                        m_kNewTriangles.add(new Point3i(kAddTri));

                        kAddTri.x = iSideIndex[1];
                        kAddTri.y = iPathIndex;
                        kAddTri.z = iNextPathIndex;
                        m_kNewTriangles.add(new Point3i(kAddTri));
                    }

                    /* Update iPathIndex: */
                    iPrevPathIndex = iPathIndex;
                    iPathIndex = iNextPathIndex;
                }
            } else {
                iNextPathIndex = iVertexCount + iNumNewVerts;

                /* Update the path point from a -1 (unknown) index to the new
                 * index value: */
                ((Point4f) kPath.get(iPath)).w = iNextPathIndex;
                iSideIndex[0] = ((Integer) kLeftPath.get(iPath)).intValue();
                iSideIndex[1] = ((Integer) kRightPath.get(iPath)).intValue();

                /* the first vertex is on the mesh, the next vertex is not on
                 * the mesh: */
                if (iPathIndex < iVertexCount) {

                    /* remove the triangle containing the first
                     * vertex and the two new sides: */
                    kDeleteTri.x = iPathIndex;
                    kDeleteTri.y = iSideIndex[0];
                    kDeleteTri.z = iSideIndex[1];

                    if (triangleExists(kDeleteTri)) {
                        m_kRemoveTriangles.add(new Point3i(kDeleteTri));

                        /* Add two new triangles: */
                        kAddTri.x = iPathIndex;
                        kAddTri.y = iSideIndex[0];
                        kAddTri.z = iNextPathIndex;
                        m_kNewTriangles.add(new Point3i(kAddTri));

                        kAddTri.x = iPathIndex;
                        kAddTri.y = iSideIndex[1];
                        kAddTri.z = iNextPathIndex;
                        m_kNewTriangles.add(new Point3i(kAddTri));

                        /* Add the next vertex: */
                        m_kNewVerts.add(new Point3f(kNextPoint.x, kNextPoint.y, kNextPoint.z));

                        /* Add the new normal: */
                        kNormal = getNormal(kMesh, iSideIndex[0], iSideIndex[1]);
                        m_kNewNormals.add(new Vector3f(kNormal));

                        /* Add the new texCoord: */
                        kTexCoord = getTexCoord(kMesh, iSideIndex[0], iSideIndex[1]);
                        m_kNewTexCoords.add(new TexCoord3f(kTexCoord));

                        /* Add the new color: */
                        kColor = getColor(kMesh, iSideIndex[0], iSideIndex[1]);
                        m_kNewColors.add(new Color4f(kColor));

                        iNumNewVerts++;
                    }

                    /* Update iPathIndex: */
                    iPrevPathIndex = iPathIndex;
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
                    kDeleteTri.x = iPrevSide;
                    kDeleteTri.y = iSideIndex[0];
                    kDeleteTri.z = iSideIndex[1];

                    if (triangleExists(kDeleteTri)) {
                        m_kRemoveTriangles.add(new Point3i(kDeleteTri));

                        /* Add three new triangles: */
                        kAddTri.x = iPathIndex;
                        kAddTri.y = iNextPathIndex;
                        kAddTri.z = iSideIndex[0];
                        m_kNewTriangles.add(new Point3i(kAddTri));

                        kAddTri.x = iPathIndex;
                        kAddTri.y = iNextPathIndex;
                        kAddTri.z = iPrevSide;
                        m_kNewTriangles.add(new Point3i(kAddTri));

                        kAddTri.x = iPrevSide;
                        kAddTri.y = iNextPathIndex;
                        kAddTri.z = iSideIndex[1];
                        m_kNewTriangles.add(new Point3i(kAddTri));

                        /* Add the next vertex: */
                        m_kNewVerts.add(new Point3f(kNextPoint.x, kNextPoint.y, kNextPoint.z));

                        /* Add the new normal: */
                        kNormal = getNormal(kMesh, iSideIndex[0], iSideIndex[1]);
                        m_kNewNormals.add(new Vector3f(kNormal));

                        /* Add the new texCoord: */
                        kTexCoord = getTexCoord(kMesh, iSideIndex[0], iSideIndex[1]);
                        m_kNewTexCoords.add(new TexCoord3f(kTexCoord));

                        /* Add the new color: */
                        kColor = getColor(kMesh, iSideIndex[0], iSideIndex[1]);
                        m_kNewColors.add(new Color4f(kColor));

                        iNumNewVerts++;
                    }

                    /* Update iPathIndex: */
                    iPrevPathIndex = iPathIndex;
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

        m_kNewVerts = new LinkedList();
        m_kNewNormals = new LinkedList();
        m_kNewTexCoords = new LinkedList();
        m_kNewColors = new LinkedList();
        m_kNewTriangles = new LinkedList();
        m_kRemoveTriangles = new LinkedList();

        /* Get the vertex counts: */
        int iVertexCount = m_kModified.getVertexCount();
        int iOldVertexCount = m_kModified.getVertexCount();

        /* First, triangulate the last Geodesic_Working path: */
        LinkedList kOpenPath = (LinkedList) m_kGeodesic_Working.get(m_iNumWorking - 1);
        LinkedList kOpenPathLeft = (LinkedList) m_kGeodesic_Working_Left.get(m_iNumWorking - 1);
        LinkedList kOpenPathRight = (LinkedList) m_kGeodesic_Working_Right.get(m_iNumWorking - 1);
        iVertexCount += triangulate(m_kModified, kOpenPath, kOpenPathLeft, kOpenPathRight, iVertexCount);

        ModelTriangleMesh kCutMesh = createNewMesh(m_kModified, iVertexCount, iOldVertexCount);

        /* Delete m_kModified, and set to kCutMesh: */
        m_kModified = null;
        m_kModified = kCutMesh;

        /* Export the modified original and the new meshes to the
         * surRenderer: */
        if (m_kPanel != null) {

            /* Backup the unmodified mesh: */
            m_kSurfaceBackup = null;
            m_kSurfaceBackup = new ModelTriangleMesh(m_kSurface);
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
     * @param   kMesh         ModelTriangleMesh surface mesh
     * @param   kPath         LinkedList path link list
     * @param   iVertexCount  int vertex count
     * @param   bOpen         boolean closed path or not
     * @param   kNewPath      LinkedList new path link list
     *
     * @return  int new number of vertices
     */
    private int unZip(ModelTriangleMesh kMesh, LinkedList kPath, int iVertexCount, boolean bOpen, LinkedList kNewPath) {
        Point3i kAddTri = new Point3i();
        Point3i kDeleteTri = new Point3i();

        Vector3f kNewNormal = new Vector3f();
        TexCoord3f kNewTexCoord = new TexCoord3f();
        Color4f kNewColor = new Color4f();
        Point4f kNewVertex = new Point4f();

        int iPathIndex;
        int iPrevPathIndex;
        int iNextPathIndex;

        LinkedList kEndList = new LinkedList();
        LinkedList kEndPoints = new LinkedList();
        int iNumEndPoints;
        int iEndIndex = -1;
        int iNewEndIndex;

        int iPath;

        boolean bAdd = true;
        Point3i kTri;

        iPath = 1;

        while (iPath < kPath.size()) {

            if (((Point4f) kPath.get(iPath - 1)).w == ((Point4f) kPath.get(iPath)).w) {
                kPath.remove(iPath);
            } else {
                iPath++;
            }
        }

        /* Add the first path index to the new list: */
        if (!bOpen && (kNewPath != null)) {
            kNewVertex = (Point4f) kPath.get(0);
            iPathIndex = (int) kNewVertex.w;
            kNewPath.add(new Integer(getPathIndex(kPath, iVertexCount, iPathIndex, bOpen)));
        }

        // for ( iPath = 1; iPath < (kPath.size() -1); iPath++ )
        for (iPath = 1; iPath < kPath.size(); iPath++) {

            /* Get the path point, index, and normal to duplicate: */
            kNewVertex = (Point4f) kPath.get(iPath);
            iPathIndex = (int) kNewVertex.w;
            kMesh.getNormal(iPathIndex, kNewNormal);
            kMesh.getTextureCoordinate(0, iPathIndex, kNewTexCoord);
            kMesh.getColor(iPathIndex, kNewColor);

            /* Add the new vertex, with a new index, the new normal to the
             * lists: */
            m_kNewVerts.add(new Point3f(kNewVertex.x, kNewVertex.y, kNewVertex.z));
            m_kNewNormals.add(new Vector3f(kNewNormal));
            m_kNewTexCoords.add(new TexCoord3f(kNewTexCoord));
            m_kNewColors.add(new Color4f(kNewColor));

            /* Add the new path index to the new list: */
            if (!bOpen && (kNewPath != null)) {
                kNewPath.add(new Integer(getPathIndex(kPath, iVertexCount, iPathIndex, bOpen)));
            }

            /* Get the previous path index: */
            iPrevPathIndex = (int) ((Point4f) kPath.get(iPath - 1)).w;

            /* Get the next path index: */
            if (iPath == (kPath.size() - 1)) {

                if (!bOpen) {
                    iNextPathIndex = (int) ((Point4f) kPath.get(1)).w;
                } else {
                    break;
                }
            } else {
                iNextPathIndex = (int) ((Point4f) kPath.get(iPath + 1)).w;
            }

            /* For each triangle that contains the edge iPrevPathIndex ->
             * iPathIndex, determine which triangle is on the "left" or
             * "right" side of the edge. */
            if (iPath == 1) {
                iNumEndPoints = findTriPoints(iPrevPathIndex, iPathIndex, kEndPoints);
                iEndIndex = ((Integer) kEndPoints.get(0)).intValue();

                if (iNumEndPoints == 2) {

                    if (onRight(kMesh, iPrevPathIndex, iPathIndex, ((Integer) kEndPoints.get(0)).intValue())) {
                        iEndIndex = ((Integer) kEndPoints.get(0)).intValue();
                    } else {
                        iEndIndex = ((Integer) kEndPoints.get(1)).intValue();
                    }
                }
            }

            kEndList.add(new Integer(iEndIndex));

            while ((iEndIndex != iNextPathIndex) && (iEndIndex != iPrevPathIndex)) {

                /* Add to the Add/Remove list: */
                kDeleteTri.x = iPrevPathIndex;
                kDeleteTri.y = iPathIndex;
                kDeleteTri.z = iEndIndex;

                if (triangleExists(kDeleteTri)) {
                    m_kRemoveTriangles.add(new Point3i(kDeleteTri));

                    kAddTri.x = getPathIndex(kPath, iVertexCount, iPrevPathIndex, bOpen);
                    kAddTri.y = getPathIndex(kPath, iVertexCount, iPathIndex, bOpen);
                    kAddTri.z = getPathIndex(kPath, iVertexCount, iEndIndex, bOpen);

                    if (!contains(m_kNewTriangles, kAddTri)) {
                        m_kNewTriangles.add(new Point3i(kAddTri));
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

                    if (((Integer) kEndPoints.get(iEnd)).intValue() != iPrevPathIndex) {
                        iPrevPathIndex = iEndIndex;
                        iEndIndex = ((Integer) kEndPoints.get(iEnd)).intValue();

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
                kDeleteTri.x = iPathIndex;
                kDeleteTri.y = iNextPathIndex;
                kDeleteTri.z = iEndIndex;

                if (triangleExists(kDeleteTri)) {
                    m_kRemoveTriangles.add(new Point3i(kDeleteTri));

                    kAddTri.x = getPathIndex(kPath, iVertexCount, iPathIndex, bOpen);
                    kAddTri.y = getPathIndex(kPath, iVertexCount, iNextPathIndex, bOpen);
                    kAddTri.z = getPathIndex(kPath, iVertexCount, iEndIndex, bOpen);

                    if (!contains(m_kNewTriangles, kAddTri)) {
                        m_kNewTriangles.add(new Point3i(kAddTri));
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
