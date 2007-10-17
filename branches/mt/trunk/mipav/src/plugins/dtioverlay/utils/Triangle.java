package dtioverlay.utils;

/**
 * Created by IntelliJ IDEA.
 * User: bennett
 * Date: Nov 20, 2005
 * Time: 9:28:30 AM
 * To change this template use Options | File Templates.
 * ************************************
 * Magnetic Resonance in Medicine Final Project
 * Released: December 1, 2005
 *
 * class Triangle
 *      Represent a triangle
 *
 * Copyright (C) 2005 Bennett Landman, bennett@bme.jhu.edu
 */
public class Triangle {
    // constant used for avoiding numerical accuracy
    static public float EPS=1e-4f;
    // set of points
    public PT pts[];
    // precompute the normal to triangle
    private PT planeNormal;
    // represent the distance from the origin to the plane of the triangle along the normal
    private float planeOffset;

    // create a new triangle
    public Triangle(PT a, PT b, PT c)  {
        pts = new PT[3];
        pts[0] = a;
        pts[1] = b;
        pts[2] = c;
        //Find plane normal
        planeNormal = (b.minus(a)).cross(c.minus(a));
        planeNormal = planeNormal.times(1f/(float)planeNormal.length());
        planeOffset = a.dot(planeNormal);
    }


/*    // detect an intersection between the triangle and the segment a->b
    public int intersect(PT a,PT b) throws DegenerateIntersectionException {
        int vol0, vol1, vol2;

        vol0 = volumeSign(a,pts[0],pts[1],b);
        vol1 = volumeSign(a,pts[1],pts[2],b);
        vol2 = volumeSign(a,pts[2],pts[0],b);

        // same sign: segment intersects interior of triangle
        if(((vol0>0)&&(vol1>0)&&(vol2>0))||((vol0<0)&&(vol1<0)&&(vol2<0)))
            return (vol0);

        // oposite sign: segment does not intersect the interior
        if(((vol0>0) || (vol1>0) || (vol2>0)) && ((vol0<0) || (vol1<0) || (vol2<0)))
            return 0;
        else if((vol0==0) && (vol1==0) && (vol2==0)) {
            // coplanar
            float dist0 = SegSegIntersect(a,b,pts[0],pts[1]);
            float dist1 = SegSegIntersect(a,b,pts[1],pts[2]);
            float dist2 = SegSegIntersect(a,b,pts[0],pts[2]);
            PT r = b.minus(a);
            if((dist0<=dist1)&&(dist0<=dist2) && (dist0<=1)) {
                PT pt = a.plus(r.times(dist0));
                if(pt.equals(pts[0]) || pt.equals(pts[1]))
                    throw new DegenerateIntersectionException("vertex intersection");
                if(dist0==0 || dist0==1)
                    throw new DegenerateIntersectionException("edge intersection");
                return 1;
            }
            if(dist1<=dist2 && dist1<=1) {
                PT pt = a.plus(r.times(dist1));
                if(pt.equals(pts[1]) || pt.equals(pts[1]))
                    throw new DegenerateIntersectionException("vertex intersection");
                if(dist0==0 || dist0==1)
                    throw new DegenerateIntersectionException("edge intersection");
                return 1;
            }
            if(dist2<=1) {
                PT pt = a.plus(r.times(dist2));
                if(pt.equals(pts[0]) || pt.equals(pts[2]))
                    throw new DegenerateIntersectionException("vertex intersection");
                if(dist0==0 || dist0==1)
                    throw new DegenerateIntersectionException("edge intersection");
                return 1;
            }
            throw new DegenerateIntersectionException("Error in Triangle.Intersect: Invalid result.");
        }

        //* two zeros: segment intersects vertex
        if(  ((vol0==0) && (vol1==0)) || ((vol0==0) && (vol2==0)) || ((vol1==0) && (vol2==0)))
            throw new DegenerateIntersectionException("vertex intersection");

        //* one zero: segment intersects edge *
        if(  ((vol0==0) ) || ((vol1==0)) || ( (vol2==0)))
            throw new DegenerateIntersectionException("vertex intersection");

        throw new DegenerateIntersectionException("Error in Triangle.Intersect: Case failure.");
    }          */

/*---------------------------------------------------------------------
    'p': The segment lies wholly within the plane.
    'q': The q endpoint is on the plane (but not 'p').
    'r': The r endpoint is on the plane (but not 'p').
    '0': The segment lies strictly to one side or the other of the plane.
    '1': The segement intersects the plane, and 'p' does not hold.
---------------------------------------------------------------------*/
    IntersectResult	SegPlaneInt( PT r, PT q)
    {

        double num, denom, t;
         PT rq =r.minus(q);


        /*printf("m=%d; plane=(%lf,%lf,%lf,%lf)\n", m, N[X],N[Y],N[Z],D);*/
        num = planeOffset - q.dot(planeNormal);

        denom = rq.dot(planeNormal);
        /*printf("SegPlaneInt: num=%lf, denom=%lf\n", num, denom );*/

        if ( denom == 0.0 ) {  /* Segment is parallel to plane. */
           if ( num == 0.0 )   /* q is on plane. */
               return new IntersectResult('p',null);
           else
               return null;
        }
        else
           t = num / denom;
        /*printf("SegPlaneInt: t=%lf \n", t );*/

        if ( (0.0 < t) && (t < 1.0) )
             return new IntersectResult('1',r.plus(rq.times((float)t)));
        else if ( num == 0.0 )   /* t == 0 */
             return new IntersectResult('q',r);
        else if ( num == denom ) /* t == 1 */
             return new IntersectResult('r',q);
        else return null;
    }

   // detect an intersection between the triangle and the segment a->b
    public boolean intersect(PT a,PT b) throws DegenerateIntersectionException    {


        IntersectResult code = SegPlaneInt(a,b);

        if      ( code.resultCode == '0')
           return false;
        else if ( code.resultCode == 'q')
           return InTri3D(code.intersectionPoint );
        else if ( code.resultCode == 'r')
           return InTri3D(code.intersectionPoint);
        else if ( code.resultCode == 'p' )
           return false; // DOES NOT CURRENTLY HANDLE ALL IN PLANE //todo: fix
        else if ( code.resultCode == '1' )
           return ('0'!=SegTriCross(a, b ));
        else /* Error */
           return false;
    }

/*---------------------------------------------------------------------
The signed volumes of three tetrahedra are computed, determined
by the segment qr, and each edge of the triangle.
Returns a char:
   'v': the open segment includes a vertex of T.
   'e': the open segment includes a point in the relative interior of an edge
   of T.
   'f': the open segment includes a point in the relative interior of a face
   of T.
   '0': the open segment does not intersect triangle T.
---------------------------------------------------------------------*/

    char SegTriCross( PT q, PT r )
    {
       int vol0, vol1, vol2;

       vol0 = volumeSign( q, pts[0], pts[1], r );
       vol1 = volumeSign( q, pts[1], pts[2], r );
       vol2 = volumeSign( q, pts[2], pts[0], r );



       /* Same sign: segment intersects interior of triangle. */
       if ( ( ( vol0 > 0 ) && ( vol1 > 0 ) && ( vol2 > 0 ) ) ||
            ( ( vol0 < 0 ) && ( vol1 < 0 ) && ( vol2 < 0 ) ) )
          return 'f';

       /* Opposite sign: no intersection between segment and triangle */
       if ( ( ( vol0 > 0 ) || ( vol1 > 0 ) || ( vol2 > 0 ) ) &&
            ( ( vol0 < 0 ) || ( vol1 < 0 ) || ( vol2 < 0 ) ) )
          return '0';

       else if ( ( vol0 == 0 ) && ( vol1 == 0 ) && ( vol2 == 0 ) )
         throw new RuntimeException( "Error 1 in SegTriCross\n" );

       /* Two zeros: segment intersects vertex. */
       else if ( ( ( vol0 == 0 ) && ( vol1 == 0 ) ) ||
                 ( ( vol0 == 0 ) && ( vol2 == 0 ) ) ||
                 ( ( vol1 == 0 ) && ( vol2 == 0 ) ) )
          return 'v';

       /* One zero: segment intersects edge. */
       else if ( ( vol0 == 0 ) || ( vol1 == 0 ) || ( vol2 == 0 ) )
          return 'e';

       else
        throw new RuntimeException( "Error 2 in SegTriCross\n" );
    }


/* Assumption: p lies in the plane containing T.
    Returns a char:
     'V': the query point p coincides with a Vertex of triangle T.
     'E': the query point p is in the relative interior of an Edge of triangle T.
     'F': the query point p is in the relative interior of a Face of triangle T.
     '0': the query point p does not intersect (misses) triangle T.
*/

    boolean 	InTri3D( PT p )
    {

       PT pp;      /* projected p */
       PT Tp[] = new PT[3];   /* projected T: three new vertices */

        if(planeNormal.x>planeNormal.z) {
            if(planeNormal.x>planeNormal.y) {
                //x max
                for(int j=0;j<3;j++)
                    Tp[j] = new PT(pts[j].y,pts[j].z,0);
                pp = new PT(p.y,p.z,0);
            } else {
                //y max
                for(int j=0;j<3;j++)
                    Tp[j] = new PT(pts[j].x,pts[j].z,0);
                pp = new PT(p.x,p.z,0);
            }


        } else {
            if(planeNormal.y>planeNormal.z) {
                // max = y;
                for(int j=0;j<3;j++)
                    Tp[j] = new PT(pts[j].x,pts[j].z,0);
                pp = new PT(p.x,p.z,0);
            } else {
                // max = z
                for(int j=0;j<3;j++)
                    Tp[j] = new PT(pts[j].x,pts[j].y,0);
                pp = new PT(p.x,p.y,0);
            }
        }
        /* Project out coordinate m in both p and the triangular face */

       return( InTri2D( Tp, pp ) );
    }

    static boolean InTri2D( PT Tp[], PT pp )
    {
       int area0, area1, area2;

       /* compute three AreaSign() values for pp w.r.t. each edge of the face in 2D */
       area0 = AreaSign( pp, Tp[0], Tp[1] );
       area1 = AreaSign( pp, Tp[1], Tp[2] );
       area2 = AreaSign( pp, Tp[2], Tp[0] );

       if ( ( area0 == 0 ) && ( area1 > 0 ) && ( area2 > 0 ) ||
            ( area1 == 0 ) && ( area0 > 0 ) && ( area2 > 0 ) ||
            ( area2 == 0 ) && ( area0 > 0 ) && ( area1 > 0 ) )
         return true;

       if ( ( area0 == 0 ) && ( area1 < 0 ) && ( area2 < 0 ) ||
            ( area1 == 0 ) && ( area0 < 0 ) && ( area2 < 0 ) ||
            ( area2 == 0 ) && ( area0 < 0 ) && ( area1 < 0 ) )
         return true;

       if ( ( area0 >  0 ) && ( area1 > 0 ) && ( area2 > 0 ) ||
            ( area0 <  0 ) && ( area1 < 0 ) && ( area2 < 0 ) )
         return true;

       if ( ( area0 == 0 ) && ( area1 == 0 ) && ( area2 == 0 ) )
         throw new RuntimeException("InTri2d Assumptions invalid.");

       if ( ( area0 == 0 ) && ( area1 == 0 ) ||
            ( area0 == 0 ) && ( area2 == 0 ) ||
            ( area1 == 0 ) && ( area2 == 0 ) )
         return true;

       else
         return false;
    }

    static int AreaSign( PT a, PT b, PT c )
    {
        double area2;

        area2 = ( b.x - a.x ) * (double)( c.y - a.y ) -
                ( c.x - a.x ) * (double)( b.y - a.y );

        /* The area should be an integer. */
        if      ( area2 >  0.5 ) return  1;
        else if ( area2 < -0.5 ) return -1;
        else                     return  0;
    }




    // find an intersection between the triangle and the segment a->b
    public IntersectResult findIntersect(PT a,PT b) throws DegenerateIntersectionException {
        PT r =b.minus(a);
        float denom = r.dot(planeNormal);
        if(denom==0) {
            if( this.planeOffset != planeNormal.dot(a)) {
                 throw new DegenerateIntersectionException("No intersection in plane");
            }
            //segment in plane of triangle
            if(this.contains(a)) {

                    return new IntersectResult(a,0,planeNormal);

            }
            float dist0 = SegSegIntersect(a,b,pts[0],pts[1]);
            float dist1 = SegSegIntersect(a,b,pts[1],pts[2]);
            float dist2 = SegSegIntersect(a,b,pts[0],pts[2]);
            if((dist0<dist1)&&(dist0<dist2))
                return new IntersectResult(a.plus(r.times(dist0)),dist0,planeNormal);
            if(dist1<dist2)
                return new IntersectResult(a.plus(r.times(dist1)),dist1,planeNormal);
            return new IntersectResult(a.plus(r.times(dist2)),dist2,planeNormal);
        }
        float t = (planeOffset-a.dot(planeNormal))/denom;
        if((t<0)||(t>1))
            throw new DegenerateIntersectionException("Error in findIntersect: No Intersection");

        PT planeIntersect =a.plus(r.times(t));
        if(planeNormal.z!=0) {
            planeIntersect.z = (planeOffset-planeIntersect.x*planeNormal.x
                    -planeNormal.y*planeIntersect.y)/planeNormal.z;
        } else {
            if(planeNormal.y!=0) {
                planeIntersect.y = (planeOffset-planeIntersect.x*planeNormal.x
                        -planeNormal.z*planeIntersect.z)/planeNormal.y;
            } else {
                planeIntersect.x = (planeOffset-planeIntersect.y*planeNormal.y
                        -planeNormal.z*planeIntersect.z)/planeNormal.x;
            }
        }

        if(contains(planeIntersect))
            return new IntersectResult(planeIntersect,t,planeNormal);
        else
            throw new DegenerateIntersectionException("No in plane intersection");
    }

    // determine the sign of the volume of the polyhedron a,b,c,d
    public int volumeSign(PT a, PT b, PT c, PT d) {
        float ax,ay,az,bx,by,bz,cx,cy,cz;
        ax = a.x-d.x;
        ay = a.y-d.y;
        az = a.z-d.z;
        bx = b.x-d.x;
        by = b.y-d.y;
        bz = b.z-d.z;
        cx = c.x-d.x;
        cy = c.y-d.y;
        cz = c.z-d.z;
        float vol = ax * (by*cz - bz*cy) +
                ay * (bz*cx - bx*cz) +
                az * (bx*cy - by*cx);
        if(vol>0)
            return 1;
        if(vol<0)
            return -1;
        return 0;
    }

/*
Calculate the line segment PaPb that is the shortest route between
two lines P1P2 and P3P4. Calculate also the values of mua and mub where
Pa = P1 + mua (P2 - P1)
Pb = P3 + mub (P4 - P3)
Returns fractional distance from p1 to p2 where it intersection p3 to p4
*/
    float SegSegIntersect(PT p1, PT p2, PT p3, PT p4)
    {
        PT p13,p43,p21;
        double d1343,d4321,d1321,d4343,d2121;
        double numer,denom;
        p13 = p1.minus(p3);
        p43 = p4.minus(p3);

        if (Math.abs(p43.x)<EPS && Math.abs(p43.y)<EPS &&  Math.abs(p43.z)<EPS)
            return Float.MAX_VALUE;
        p21 = p2.minus(p1);

        if (Math.abs(p21.x)<EPS && Math.abs(p21.y)<EPS &&  Math.abs(p21.z)<EPS)
            return Float.MAX_VALUE;

        d1343 = p13.x * p43.x + p13.y * p43.y + p13.z * p43.z;
        d4321 = p43.x * p21.x + p43.y * p21.y + p43.z * p21.z;
        d1321 = p13.x * p21.x + p13.y * p21.y + p13.z * p21.z;
        d4343 = p43.x * p43.x + p43.y * p43.y + p43.z * p43.z;
        d2121 = p21.x * p21.x + p21.y * p21.y + p21.z * p21.z;

        denom = d2121 * d4343 - d4321 * d4321;
        if (Math.abs(denom) < EPS)
            return Float.MAX_VALUE;
        numer = d1343 * d4321 - d1321 * d4343;

        double mua = numer / denom;
        double mub = (d1343 + d4321 * mua) / d4343;

        if(mub>=0 && mub<=1 && mua>=0 && mua<=1)
            return (float)mua;

        return Float.MAX_VALUE;

    }

    // determine if a and b are on the same side of p1->p2
    private boolean sameside(PT p1, PT p2, PT a, PT b) {
        PT ba = b.minus(a);
        PT cp1 = ba.cross(p1.minus(a));
        PT cp2 = ba.cross(p2.minus(a));
        return (cp1.dot(cp2)>=0);
    }

    // determine if this triangle contains a point
    public boolean contains(PT p) {
        return ( sameside(p,pts[0],pts[1],pts[2]) &&
                sameside(p,pts[1],pts[2],pts[0]) &&
                sameside(p,pts[2],pts[0],pts[1]));
    }
}
