package gov.nih.mipav.model.structures;

import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;



import java.awt.Polygon;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

import Jama.Matrix;
import WildMagic.LibFoundation.Approximation.ApprEllipsoidFit3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

public class GenericPolygonClipper {
    
    // This is a port of GenericPolygonClipper from C into Java.  Below
    // is the statement that comes with the original C gpc.c file.
    
    /**Project:   Generic Polygon Clipper

    A new algorithm for calculating the difference, intersection,
    exclusive-or or union of arbitrary polygon sets.

File:      gpc.c
Author:    Alan Murta (email: gpc@cs.man.ac.uk)
Version:   2.32
Date:      17th December 2004

Copyright: (C) Advanced Interfaces Group,
    University of Manchester.

    This software is free for non-commercial use. It may be copied,
    modified, and redistributed provided that this copyright notice
    is preserved on all copies. The intellectual property rights of
    the algorithms used reside with the University of Manchester
    Advanced Interfaces Group.

    You may not use this software, in whole or in part, in support
    of any commercial product without the express consent of the
    author.

    There is no warranty or other guarantee of fitness of this
    software for any purpose. It is provided solely "as is".*/
    
    private static final int LEFT = 0;
    private static final int RIGHT = 1;
    
    private static final int ABOVE = 0;
    private static final int BELOW = 1;
    
    private static final int CLIP = 0;
    private static final int SUBJ = 1;

    public enum gpc_op {
        // Set operation type
        GPC_DIFF, // Difference
        GPC_INT, // Intersection
        GPC_XOR, // Exclusive or
        GPC_UNION; // Union
    }
    
    private enum h_state {
        // Horizontal edge states
        NH, // No horizontal edge
        BH, // Bottom horizontal edge
        TH; // Top horizontal edge
    }
    
    private enum bundle_state {
        // Edge bundle state
        UNBUNDLED, // Isolated edge not within a bundle
        BUNDLE_HEAD, // Bundle head node
        BUNDLE_TAIL; // Passive bundle tail node
    }
    
    private class gpc_vertex {
        // Polygon vertex structure
        double x; // Vertex x component
        double y; // Vertex y component
        
        public void setXY(double x, double y) {
            this.x = x;
            this.y = y; 
        }
    }
    
    private class gpc_vertex_list {
        // Vertex list structure
        int  num_vertices; // Number of vertices in list
        gpc_vertex vertex[]; // Vertex array pointer
        
        public void setNumVertices(int num_vertices) {
            this.num_vertices = num_vertices;
        }
        
        public void setVertex(gpc_vertex vertex[]) {
            this.vertex = vertex;
        }
    }
    
    private class sb_tree {
        // Scanbeam tree
        double y; // Scanbeam node y value
        sb_tree less; // Pointer to nodes with lower y
        sb_tree more; // Pointer to nodes with higher y
    }
    
    private class vertex_node {
        // Internal vertex list
        double x; // X coordinate component
        double y; // Y coordinate component
        vertex_node next; // Pointer to next vertex in list
    }
    
    private class polygon_node {
        // Internal contour / tristripe type
        int active; // Active flag / vertex count
        int hole; // Hole / external contour flag
        vertex_node v[] = new vertex_node[2]; // Left and right vertex node pointers
        polygon_node next; // Pointer to next polygon contour
        polygon_node proxy; // Pointer to actual structure used
    }
    
    private class edge_node {
        gpc_vertex vertex = new gpc_vertex(); // Piggy-backed contour vertex data
        gpc_vertex bot = new gpc_vertex(); // Edge lower (x, y) coordinate
        gpc_vertex top = new gpc_vertex(); // Edge upper (x, y) coordinate
        double xb; // Scanbeam bottom x coordinate
        double xt; // Scanbeam top x coordinate
        double dx; // Change in x for a unit y increase
        int type; // Clip / subject edge flag
        boolean bundle[][] = new boolean[2][2]; // Bundle edge flags
        int bside[] = new int[2]; // Bundle left / right indicators
        bundle_state bstate[] = new bundle_state[2]; // Edge bundle state
        polygon_node outp[] = new polygon_node[2]; // Output polygon / tristrip pointer
        edge_node prev; // Previous edge in the AET
        edge_node next; // Next edge in the AET
        edge_node pred; // Edge connected at the lower end
        edge_node succ; // Edge connected at the upper end
        edge_node next_bound[] = new edge_node[1]; // Pointer to the next bound in LMT
    }
    
    private class lmt_node {
        // Local minima table
        double y; // Y coordinate at local minimum
        edge_node first_bound[] = new edge_node[1]; // Pointer to bound list
        lmt_node next; // Pointer to next local minimum
    }
    
    private class it_node {
        // Intersection table
        edge_node ie[] = new edge_node[2]; // Intersecting edge (bundle) pair
        gpc_vertex point; // Point of intersection
        it_node next; // The next intersection table node
    }
    
    private class bbox {
        // Contour axis-aligned bounding box
        double xmin; // Minimum x coordinate
        double ymin; // Minimum y coordinate
        double xmax; // Maximum x coordinate
        double ymax; // Maximum y coordinate
    }
    
    private boolean clipContributingStatus[];
    private boolean subjContributingStatus[];
    
    private int PREV_INDEX(int i, int n) {
        return ((i - 1 + n) % n);
    }
    
    private int NEXT_INDEX(int i, int n) {
        return ((i + 1) % n);
    }
    
    private boolean OPTIMAL(gpc_vertex v[], int i, int n) {
        return ((v[PREV_INDEX(i, n)].y != v[i].y) ||
                (v[NEXT_INDEX(i, n)].y != v[i].y));
    }
    
    private boolean FWD_MIN(edge_node v[], int i, int n) {
        return ((v[PREV_INDEX(i, n)].vertex.y >= v[i].vertex.y) &&
                (v[NEXT_INDEX(i, n)].vertex.y > v[i].vertex.y));
    }
    
    private boolean NOT_FMAX(edge_node v[], int i, int n) {
        return (v[NEXT_INDEX(i, n)].vertex.y > v[i].vertex.y);
    }
    
    private boolean REV_MIN(edge_node v[], int i, int n) {
        return ((v[PREV_INDEX(i, n)].vertex.y > v[i].vertex.y) &&
                (v[NEXT_INDEX(i, n)].vertex.y >= v[i].vertex.y));
    }
    
    private boolean NOT_RMAX(edge_node v[], int i, int n) {
        return (v[PREV_INDEX(i, n)].vertex.y > v[i].vertex.y);
    }
    
    private void reset_lmt(lmt_node lmt) {
        lmt_node lmtn;
        while (lmt != null) {
            lmtn = lmt.next;
            lmt = null;
            lmt = lmtn;
        }
    }
    
    private void insert_bound(edge_node b[], edge_node e[], int index) {
        edge_node existing_bound;
        if (b[0] == null) {
            // Link node e to the tail of the list
            b[0] = e[index];
        }
        else {
            // Do primary sort on the x field
            if (e[index].bot.x < b[0].bot.x) {
                // Insert a new node mid-list
                existing_bound = b[0];
                b[0] = e[index];
                b[0].next_bound[0] = existing_bound;
            }
            else {
                if (e[index].bot.x == b[0].bot.x) {
                    // Do secondary sort on the dx field
                    if (e[index].dx < b[0].dx) {
                        // Insert a new node mid-list
                        existing_bound = b[0];
                        b[0] = e[index];
                        b[0].next_bound[0] = existing_bound;
                    }
                    else {
                        // Head further down the list
                        insert_bound(b[0].next_bound, e, index);
                    }
                }
                else {
                    // Head further down the list
                    insert_bound(b[0].next_bound, e, index);
                }
            }
        }
    }
    
    private edge_node[] bound_list(lmt_node lmt, double y) {
        lmt_node existing_node;
        if (lmt == null) {
            // Add node onto the tail end of the LMT */
            lmt = new lmt_node();
            lmt.y = y;
            lmt.first_bound[0] = null;
            lmt.next = null;
            return lmt.first_bound;
        }
        else {
            if (y < lmt.y) {
                // Insert a new LMT node before the current node
                existing_node = lmt;
                lmt = new lmt_node();
                lmt.y = y;
                lmt.first_bound[0] = null;
                lmt.next = existing_node;
                return lmt.first_bound;
            }
            else {
                if (y > lmt.y) {
                    // Head further up the lmt
                    return bound_list(lmt.next, y);
                }
                else {
                    // Use this existing LMT node
                    return lmt.first_bound;
                }
            }
        }
    }
    
    private void add_to_sbtree(int entries[], sb_tree sbtree, double y) {
        if (sbtree == null) {
            sbtree = new sb_tree(); 
            sbtree.y = y;
            sbtree.less = null;
            sbtree.more = null;
            entries[0]++;
        }
        else {
            if (sbtree.y > y) {
                // Head into the 'less' sub-tree
                add_to_sbtree(entries, sbtree.less, y);
            }
            else {
                if (sbtree.y < y) {
                    // Head into the 'more' sub-tree
                    add_to_sbtree(entries, sbtree.more, y);
                }
            }
        }
    }
    
    private void build_sbt(int entries[], double sbt[], sb_tree sbtree) {
        if (sbtree.less != null) {
            build_sbt(entries, sbt, sbtree.less);
        }
        sbt[entries[0]] = sbtree.y;
        entries[0]++;
        if (sbtree.more != null) {
            build_sbt(entries, sbt, sbtree.more);
        }
    }
    
    private void free_sbtree(sb_tree sbtree) {
        if (sbtree != null) {
            free_sbtree(sbtree.less);
            free_sbtree(sbtree.more);
            sbtree = null;
        }
    }
    
    private int count_optimal_vertices(gpc_vertex_list c) {
        int result = 0;
        int i;
        
        // Ignore non-contributing contours already take care of in build_lmt
        for (i = 0; i < c.num_vertices; i++) {
            // Ignore superfluous vertices embedded in horizontal edges
            if (OPTIMAL(c.vertex, i, c.num_vertices)) {
                result++;
            }
        }
        
        return result;
    }
    
    private edge_node[] build_lmt(lmt_node lmt, sb_tree sbtree, int[] sbt_entries, VOIBaseVector p, int type, gpc_op op) {
        int c;
        int i;
        int min;
        int max;
        int num_edges;
        int v;
        int num_vertices;
        int total_vertices = 0;
        int e_index = 0;
        edge_node e[];
        edge_node edge_table[];
        gpc_vertex vertex[];
        gpc_vertex_list vl;
        int index;
        
        for (c = 0; c < p.size(); c++) {
            if (((type == SUBJ) && (!subjContributingStatus[c])) || ((type == CLIP) && (!clipContributingStatus[c]))) {
                continue;    
            }
            num_vertices = p.get(c).size();
            vertex = new gpc_vertex[num_vertices];
            for (i = 0; i < num_vertices; i++) {
                vertex[i] = new gpc_vertex();
                vertex[i].setXY(p.get(c).get(i).X, p.get(c).get(i).Y);
            }
            vl = new gpc_vertex_list();
            vl.setNumVertices(num_vertices);
            vl.setVertex(vertex);
            total_vertices += count_optimal_vertices(vl);
        }
        
        // Create the entire polygon edge table in one go
        edge_table = new edge_node[total_vertices];
        for (i = 0; i < total_vertices; i++) {
            edge_table[i] = new edge_node();
        }
        
        for (c = 0; c < p.size(); c++) {
            if ((type == SUBJ) && (!subjContributingStatus[c])) {
                // Ignore the non-contributing contour and change contributing status to true
                subjContributingStatus[c] = true;
            }
            else if ((type == CLIP) && (!clipContributingStatus[c])) {
                // Ignore the non-contributing contour and change contributing status to true
                clipContributingStatus[c] = true;
            }
            else {
                // Perform contour optimization
                num_vertices = 0;
                vertex = new gpc_vertex[p.get(c).size()];  
                for (i = 0; i < p.get(c).size(); i++) {
                    vertex[i] = new gpc_vertex();   
                    vertex[i].setXY(p.get(c).get(i).X, p.get(c).get(i).Y);
                } // for (i = 0; i < p.get(c).size(); i++)
                for (i = 0; i < p.get(c).size(); i++) {
                    if (OPTIMAL(vertex, i, p.get(c).size())) {
                        edge_table[num_vertices].vertex.x = vertex[i].x;
                        edge_table[num_vertices].vertex.y = vertex[i].y;
                        // Record vertex in the scanbeam table
                        add_to_sbtree(sbt_entries, sbtree, edge_table[num_vertices].vertex.y);
                        num_vertices++;
                    }
                }
                    
                // Do the contour forward pass
                for (min = 0; min < num_vertices; min++) {
                    // If a forward local minimum ...
                    if (FWD_MIN(edge_table, min, num_vertices)) {
                        // Search for the next local maximum...
                        num_edges = 1;
                        max = NEXT_INDEX(min, num_vertices);
                        while (NOT_FMAX(edge_table, max, num_vertices)) {
                            num_edges++;
                            max = NEXT_INDEX(max, num_vertices);
                        }
                        
                        // Build the next edge list
                        index = e_index;
                        e_index += num_edges;
                        v = min;
                        edge_table[index].bstate[BELOW] = bundle_state.UNBUNDLED;
                        edge_table[index].bundle[BELOW][CLIP] = false;
                        edge_table[index].bundle[BELOW][SUBJ] = false;
                        for (i = 0; i < num_edges; i++) {
                            edge_table[index + i].xb = edge_table[v].vertex.x;
                            edge_table[index + i].bot.x = edge_table[v].vertex.x;
                            edge_table[index + i].bot.y = edge_table[v].vertex.y;
                            
                            v = NEXT_INDEX(v, num_vertices);
                            
                            edge_table[index + i].top.x = edge_table[v].vertex.x;
                            edge_table[index + i].top.y = edge_table[v].vertex.y;
                            edge_table[index + i].dx = (edge_table[v].vertex.x - edge_table[index + i].bot.x)/
                                                       (edge_table[index + i].top.y - edge_table[index + i].bot.y);
                            edge_table[index + i].type = type;
                            edge_table[index + i].outp[ABOVE] = null;
                            edge_table[index + i].outp[BELOW] = null;
                            edge_table[index + i].next = null;
                            edge_table[index + i].prev = null;
                            edge_table[index + i].succ = ((num_edges > 1) && (i < (num_edges-1))) ? edge_table[index + i + 1] : null; 
                            edge_table[index + i].pred = ((num_edges > 1) && (i > 0)) ? edge_table[index + i - 1] : null;
                            edge_table[index + i].next_bound[0] = null;
                            edge_table[index + i].bside[CLIP] = (op == gpc_op.GPC_DIFF) ? RIGHT : LEFT;
                            edge_table[index + i].bside[SUBJ] = LEFT;
                        } // for (i = 0; i < num_edges; i++)
                        insert_bound(bound_list(lmt, edge_table[min].vertex.y), edge_table, index);
                    } // if (FWD_MIN(edge_table, min, num_vertices))
                } // for (min = 0; min < num_vertices; min++)
                
                // Do the contour reverse pass
                for (min = 0; min < num_vertices; min++) {
                    // If a reverse local minimum...
                    if (REV_MIN(edge_table, min, num_vertices)) {
                        // Search for the previous local maximum...
                        num_edges = 1;
                        max = PREV_INDEX(min, num_vertices);
                        while (NOT_RMAX(edge_table, max, num_vertices)) {
                            num_edges++;
                            max = PREV_INDEX(max, num_vertices);
                        }
                        
                        // Build the previous edge list
                        index = e_index;
                        e_index += num_edges;
                        v = min;
                        edge_table[index].bstate[BELOW] = bundle_state.UNBUNDLED;
                        edge_table[index].bundle[BELOW][CLIP] = false;
                        edge_table[index].bundle[BELOW][SUBJ] = false;
                        for (i = 0; i < num_edges; i++) {
                            edge_table[index + i].xb = edge_table[v].vertex.x;
                            edge_table[index + i].bot.x = edge_table[v].vertex.x;
                            edge_table[index + i].bot.y = edge_table[v].vertex.y;
                            
                            v = PREV_INDEX(v, num_vertices);
                            
                            edge_table[index + i].top.x = edge_table[v].vertex.x;
                            edge_table[index + i].top.y = edge_table[v].vertex.y;
                            edge_table[index + i].dx = (edge_table[v].vertex.x - edge_table[index + i].bot.x)/
                                                       (edge_table[index + i].top.y - edge_table[index + i].bot.y);
                            edge_table[index + i].type = type;
                            edge_table[index + i].outp[ABOVE] = null;
                            edge_table[index + i].outp[BELOW] = null;
                            edge_table[index + i].next = null;
                            edge_table[index + i].prev = null;
                            edge_table[index + i].succ = ((num_edges > 1) && (i < (num_edges - 1))) ? edge_table[index + i + 1] : null;
                            edge_table[index + i].pred = ((num_edges > 1) && (i > 0)) ? edge_table[index + i - 1] : null;
                            edge_table[index + i].next_bound[0] = null;
                            edge_table[index + i].bside[CLIP] = (op == gpc_op.GPC_DIFF) ? RIGHT : LEFT;
                            edge_table[index + i].bside[SUBJ] = LEFT;
                        } // for (i = 0; i < num_edges; i++)
                        insert_bound(bound_list(lmt, edge_table[min].vertex.y), edge_table, index);
                    } // if (REV_MIN(edge_table, min, num_vertices))
                } // for (min = 0; min < num_vertices; min++)
            } // else
        } // for (c = 0; c < p.size; c++)
        
        return edge_table;
    }
    
    private void add_edge_to_aet(edge_node aet, edge_node edge, edge_node prev) {
        if (aet == null) {
            // Append edge onto the tail end of the AET
            aet = edge;
            edge.prev = prev;
            edge.next = null;
        }
        else {
            // Do primary sort on the xb field
            if (edge.xb < aet.xb) {
                // Insert edge here (before the AET edge)
                edge.prev = prev;
                edge.next = aet;
                aet.prev = edge;
                aet = edge;
            }
            else {
                if (edge.xb == aet.xb) {
                    // Do secondary sort on the dx field
                    if (edge.dx < aet.dx) {
                        // Insert edge here (before the AET edge)
                        edge.prev = prev;
                        edge.next = aet;
                        aet.prev = edge;
                        aet = edge;
                    }
                    else {
                        // Head further into the AET
                        add_edge_to_aet(aet.next, edge, aet);
                    }
                }
                else {
                    // Head further into the AET
                    add_edge_to_aet(aet.next, edge, aet);
                }
            }
        }
    }
    
    private bbox[] create_contour_bboxes(VOIBaseVector p) {
        bbox[] box;
        int c;
        int v;
        
        box = new bbox[p.size()];
        
        // Construct contour bounding boxes
        for (c = 0; c < p.size(); c++) {
            box[c] = new bbox();
            // Initialize bounding box extent
            box[c].xmin = Double.MAX_VALUE;
            box[c].ymin = Double.MAX_VALUE;
            box[c].xmax = -Double.MAX_VALUE;
            box[c].ymax = -Double.MAX_VALUE;
            
            for (v = 0; v < p.get(c).size(); v++) {
                // Adjust bounding box
                if (p.get(c).get(v).X < box[c].xmin) {
                    box[c].xmin = p.get(c).get(v).X;
                }
                if (p.get(c).get(v).Y < box[c].ymin) {
                    box[c].ymin = p.get(c).get(v).Y;
                }
                if (p.get(c).get(v).X > box[c].xmax) {
                    box[c].xmax = p.get(c).get(v).X;
                }
                if (p.get(c).get(v).Y > box[c].ymax) {
                    box[c].ymax = p.get(c).get(v).Y;
                }
            } 
        } // for (c = 0; c < pCurves.get(c).size(); c++)
        return box;
    }
    
    private void minimax_test(VOIBaseVector subj, VOIBaseVector clip, gpc_op op) {
        bbox s_bbox[];
        bbox c_bbox[];
        int s;
        int c;
        boolean o_table[];
        boolean overlap;
        
        subjContributingStatus = new boolean[subj.size()];
        for (s = 0; s < subj.size(); s++) {
            subjContributingStatus[s] = true;
        }
        clipContributingStatus = new boolean[clip.size()];
        for (c = 0; c < clip.size(); c++) {
            clipContributingStatus[c] = true;
        }
        
        s_bbox = create_contour_bboxes(subj);
        c_bbox = create_contour_bboxes(clip);
        
        o_table = new boolean[subj.size()*clip.size()];
        
        // Check all subject contour bounding boxes against clip boxes
        for (s = 0; s < subj.size(); s++) {
            for (c = 0; c < clip.size(); c++) {
                o_table[c*subj.size() + s] =
                       (!((s_bbox[s].xmax < c_bbox[c].xmin) ||
                          (s_bbox[s].xmin > c_bbox[c].xmax))) &&
                       (!((s_bbox[s].ymax < c_bbox[c].ymin) ||
                          (s_bbox[s].ymin > c_bbox[c].ymax)));
            } // for (c = 0; c < clip.size(); c++)
        } // for (s = 0; s < subj.size(); s++)
        
        // For each clip contour, search for any subject contour overlaps
        for (c = 0; c < clip.size(); c++) {
            overlap = false;
            for (s = 0; (!overlap) && (s < subj.size()); s++) {
                overlap = o_table[c * subj.size() + s];    
            } // for (s = 0; (!overlap) && (s < subj.size()); s++)
            
            if (!overlap) {
                // Flag non contributing status
                clipContributingStatus[c] = false;
            }
        } // for (c = 0; c < clip.size(); c++)
        
        if (op == gpc_op.GPC_INT) {
            // For each subject contour, search for any clip contour overlaps
            for (s = 0; s < subj.size(); s++) {
                overlap = false;
                for (c = 0; (!overlap) && (c < clip.size()); c++) {
                    overlap = o_table[c*subj.size() + s];
                } // for (c = 0; (!overlap) && (c < clip.size(); c++)
                
                if (!overlap) {
                    // Flag non contributing status
                    subjContributingStatus[s] = false;
                }
            } // for (s = 0; s < subj.size(); s++)
        } // if (op == gpc_op.GPC_INT)
        
        s_bbox = null;
        c_bbox = null;
        o_table = null;
    }
    
    

    
    public GenericPolygonClipper(gpc_op op, VOI subj, VOI clip,
                                 VOI result) {
        
        if (subj.getCurveType() != VOI.CONTOUR) {
            MipavUtil.displayError("subj is not a VOI.CONTOUR");
            return;
        }
        
        if (clip.getCurveType() != VOI.CONTOUR) {
            MipavUtil.displayError("clip is not a VOI.CONTOUR");
            return;
        }
        
        if (result.getCurveType() != VOI.CONTOUR) {
            MipavUtil.displayError("result is not a VOI.CONTOUR");
            return;
        }
        
        sb_tree sbtree = null;
        it_node it[] = null;
        it_node intersect[] = new it_node[]{new it_node()};
        edge_node edge;
        edge_node prev_edge[] = new edge_node[]{new edge_node()};
        edge_node next_edge[] = new edge_node[]{new edge_node()};
        edge_node succ_edge[] = new edge_node[]{new edge_node()};
        edge_node e0[] = new edge_node[]{new edge_node()};
        edge_node e1[] = new edge_node[]{new edge_node()};
        edge_node aet = null;
        edge_node c_heap[] = null;
        edge_node s_heap[] = null;
        lmt_node lmt = null;
        lmt_node local_min;
        polygon_node outpoly[] = null;
        polygon_node p[] = new polygon_node[]{new polygon_node()};
        polygon_node q[] = new polygon_node[]{new polygon_node()};
        polygon_node poly[] = new polygon_node[]{new polygon_node()};
        polygon_node npoly[] = new polygon_node[]{new polygon_node()};
        polygon_node cf[] = null;
        vertex_node vtx[] = new vertex_node[]{new vertex_node()};
        vertex_node nv[] = new vertex_node[]{new vertex_node()};
        h_state horiz[] = new h_state[2];
        int in[] = new int[2];
        int exists[] = new int[2];
        int parity[] = new int[]{LEFT, LEFT};
        int c;
        int v;
        int contributing;
        int search;
        int scanbeam[] = new int[]{0};
        int sbt_entries[] = new int[]{0};
        int vclass = 0;
        int bl;
        int br;
        int tl;
        int tr;
        double sbt[] = null;
        double xb;
        double px;
        double yb;
        double yt;
        double dy;
        double ix;
        double iy;
        VOIBaseVector subjCurves = subj.getCurves();
        VOIBaseVector clipCurves = clip.getCurves();
        
        // Test for trivial null result cases
        if (((subjCurves.size() == 0) || (clipCurves.size() == 0))
         || ((subjCurves.size() == 0) && ((op == gpc_op.GPC_INT) || (op == gpc_op.GPC_DIFF)))
         || ((clipCurves.size() == 0) && (op == gpc_op.GPC_INT))) {
             return;    
         }
        
        // Identify potentially contributing contours
        if (((op == gpc_op.GPC_INT) || (op == gpc_op.GPC_DIFF))
         && (subjCurves.size() > 0) && (clipCurves.size() > 0)) {
            minimax_test(subjCurves, clipCurves, op);
        }
        
        // Build LMT
        if (subjCurves.size() > 0) {
            s_heap = build_lmt(lmt, sbtree, sbt_entries, subjCurves, SUBJ, op);
        }
        if (clipCurves.size() > 0) {
            c_heap = build_lmt(lmt, sbtree, sbt_entries, clipCurves, CLIP, op);
        }
        
        // Return a null result if no contours contribute
        if (lmt == null) {
            s_heap = null;
            c_heap = null;
            System.out.println("lmt == null");
            return;
        }
        
        // Build scanbeam table from scanbeam tree
        sbt = new double[sbt_entries[0]];
        build_sbt(scanbeam, sbt, sbtree);
        scanbeam[0] = 0;
        free_sbtree(sbtree);
        
        // Invert clip polygon for difference operation
        if (op == gpc_op.GPC_DIFF) {
            parity[CLIP] = RIGHT;
        }
        
        local_min = lmt;
        
        // Process each scanbeam
        while (scanbeam[0] < sbt_entries[0]) {
          // Set yb and yt to the bottom and top of the scanbeam
            yb = sbt[scanbeam[0]++];
            if (scanbeam[0] < sbt_entries[0]) {
                yt = sbt[scanbeam[0]];
                dy = yt - yb;
            }
            
            // SCANBEAM BOUNDARY PROCESSING
            
            // If LMT node corresponding to yb exists
            if (local_min != null) {
                if (local_min.y == yb) {
                    // Add edges starting at this local minimum to the AET
                    for (edge = local_min.first_bound[0]; edge != null; edge = edge.next_bound[0]) {
                        add_edge_to_aet(aet, edge, null);
                    }
                } // if (local_min.y == yb)
            } // if (local_min != null)
        } // while (scanbeam[0] < sbt_entries[0])
        System.out.println("I did GPC");
    } 
    
}