
package gov.nih.mipav.util;

import java.util.ArrayList;

/**
 * Public utility class for generating approximations to circles and spheres.
 * 
 * @see McIlroy, M. D.  Best approximate circles on integer grids. ACM Transactions on Graphics (TOG),  Volume 2 Issue 4. 
 * @see http://portal.acm.org/citation.cfm?id=246, DOI: http://doi.acm.org/10.1145/245.246
 * 
 * @author senseneyj
 *
 */

public class CircleUtil {
    
    public static int[][] get3DPointsInSphere(int xCenter, int yCenter, int zCenter, double xRes, double yRes, double zRes, double radius) {
        double largestRes = xRes > yRes ? (xRes > zRes ? xRes : zRes) : (yRes > zRes ? yRes : zRes);
        double smallestRes = xRes < yRes ? (xRes < zRes ? xRes : zRes) : (yRes < zRes ? yRes : zRes);
        
        int pixRadius = (int) Math.round(radius / smallestRes);
        int[][] sphere = CircleUtil.get3DPointsInSphere(xCenter, yCenter, zCenter, pixRadius);
        
        for(int i=0; i<sphere.length; i++) {
            //System.out.print(sphere[i][0]+", "+sphere[i][1]+", "+sphere[i][2]+" changed to ");
            sphere[i][0] = (int) Math.round(((smallestRes/xRes)*(sphere[i][0]-xCenter))+xCenter);
            sphere[i][1] = (int) Math.round(((smallestRes/yRes)*(sphere[i][1]-yCenter))+yCenter);
            sphere[i][2] = (int) Math.round(((smallestRes/zRes)*(sphere[i][2]-zCenter))+zCenter);
            //System.out.println(sphere[i][0]+", "+sphere[i][1]+", "+sphere[i][2]);
        }
        
        return sphere;
    }
    
    
    /**
     * 
     * Calculates the points of an approximate sphere whose center is at <code>(xCenter, yCenter, zCenter)</code>
     * witha given radius.
     * 
     * @return  Array of (x,y,z) coordinates of an approximate sphere.
     */
    
    public static int[][] get3DPointsInSphere(int xCenter, int yCenter, int zCenter, int radius) {
        if(radius>1) {
            int[][] quadrant1 = getQuadrantBoundaryPoints(radius, 1);
            int[][] quadrant2 = getQuadrantBoundaryPoints(radius, 2);
            //int j = 0, k=0;
            int[][][] slices = new int[2*radius+1][][];
            int placeHolder = 0;    //shouldn't be necessary
            for(int i=0; i<quadrant1.length-1; i++) {
               if(!(quadrant1[i][0] == quadrant1[i+1][0])) {
                   int radiusTemp =  quadrant1[i][1];
                   slices[placeHolder] = get2DPointsInCircle(yCenter, zCenter, radiusTemp);
                   placeHolder++;
               }
            }
            slices[placeHolder] = get2DPointsInCircle(yCenter, zCenter, radius);
            placeHolder++;
            slices[placeHolder] = get2DPointsInCircle(yCenter, zCenter, radius);
            placeHolder++;
            for(int i=1; i<quadrant2.length; i++)
            {
                if(!(quadrant2[i][0] == quadrant2[i-1][0]))
                {
                    slices[placeHolder] = get2DPointsInCircle(yCenter, zCenter, Math.abs(quadrant2[i][1]));
                    placeHolder++;
                }
            }
            int size = 0;
            for(int i=0; i<slices.length; i++)
                size = size+slices[i].length;
            int[][] spherePoints = new int[size][3];
            int place = 0;
            for(int i=0; i<slices.length; i++) {
                for(int n=0; n<slices[i].length; n++) {
                    spherePoints[place][0] = xCenter+i-radius;
                    spherePoints[place][1] = slices[i][n][0];
                    spherePoints[place][2] = slices[i][n][1];
                    place++;
                }
            }
            return spherePoints;
        }
       
        //r <=1
        int[][] spherePoints = new int[1][3];
        spherePoints[0][0] = xCenter;
        spherePoints[0][1] = yCenter;
        spherePoints[0][2] = zCenter;
        return spherePoints;
    }
    
    /**
     * 
     * Calculates the points of an approximate circle whose center is at <code>(xCenter, yCenter)</code>
     * with a given radius.
     * 
     * @return  Array of (x,y) coordinates of an approximate circle.
     */
    
    public static int[][] get2DPointsInCircle(int xCenter, int yCenter, int r) {
        if(r>1) {
            ArrayList<int[]> points = new ArrayList<int[]>();
            int[] corner = new int[2];
            int[][] quadrant1 = getQuadrantBoundaryPoints(r, 1);
            int[][] quadrant3 = getQuadrantBoundaryPoints(r, 3);
            for(int i=0; i<quadrant1.length; i++) {
                if((i==0) || !(quadrant1[i-1][1] == quadrant1[i][1])) {
                    corner = quadrant1[i];
                    for(int j=corner[0]; j>=-corner[0]; j--) {
                        int[] tempPoint = new int[2];
                        tempPoint[0] = xCenter + j;
                        tempPoint[1] = yCenter + corner[1];
                        points.add(tempPoint);
                    }
                }
            }
            for(int i=1; i<quadrant3.length; i++) {
                if(!(quadrant3[i-1][1] == quadrant3[i][1])) {
                    corner = quadrant3[i];
                    for(int j=-corner[0]; j>=corner[0]; j--) {
                        int[] tempPoint = new int[2];
                        tempPoint[0] = xCenter + j;
                        tempPoint[1] = yCenter + corner[1];
                        points.add(tempPoint);
                    }
                }
            }
            int[][] circlePoints = new int[points.size()][2];
            for(int i=0; i<points.size(); i++) {
                circlePoints[i] = (int[])points.get(i);
            }
            return circlePoints;
        }
        
        //r <= 1
        int[][] circlePoints = new int[1][2];
        circlePoints[0][0] = xCenter;
        circlePoints[0][1] = yCenter;
        return circlePoints;
    }
    
    /**
     * 
     * Calculates the points of an approximate sphere whose center is at <code>(xCenter, yCenter, zCenter)</code>
     * with a given radius.
     * 
     * @return  Array of points of an approximate sphere.
     */
    
    public static int[] get1DPointsInSphere(int xCenter, int yCenter, int zCenter, int radius, int xDim, int yDim) {
        int[][] spherePoints = get3DPointsInSphere(xCenter, yCenter, zCenter, radius);
        int[] convertedPoints = new int[spherePoints.length];
        for(int i=0; i<spherePoints.length; i++) {
            convertedPoints[i] = spherePoints[i][2]*(xDim*yDim)+spherePoints[i][1]*xDim + spherePoints[i][0];
        }
        return convertedPoints;
    }
    
    /**
     * 
     * Calculates the points of an approximate sphere whose center is at <code>value</code>
     * with a given radius.
     * 
     * @return  Array of points of an approximate sphere.
     */
    
    public static int[] get1DPointsInSphere(int value, int radius, int xDim, int yDim) {
        int z = value / (xDim*yDim);
        int y = (value - z*(xDim*yDim)) / xDim;
        int x = value - z*(xDim*yDim) - y*xDim;
        return get1DPointsInSphere(x, y, z, radius, xDim, yDim);
    }
    
    /**
     * 
     * Calculates the points of an approximate sphere whose center is at <code>(xCenter, yCenter)</code>
     * with a given radius.
     * 
     * @return  Array of points of an approximate sphere.
     */
    
    public static int[] get1DPointsInCircle(int xCenter, int yCenter, int zCenter, int radius, int dimX, int dimY) {
        int[][] circlePoints = get2DPointsInCircle(xCenter, yCenter, radius);
        int[] convertedPoints = new int[circlePoints.length];
        for(int i=0; i<circlePoints.length; i++) {
           convertedPoints[i] = zCenter*(dimX*dimY) + circlePoints[i][1]*dimX + circlePoints[i][0];
        }
        return convertedPoints;    
    }
    
    /**
     * Generates 2D data points in a given  quadrant for a specified <code>radius</code>
     * 
     * @see McIlroy, M. D.  Best approximate circles on integer grids. ACM Transactions on Graphics (TOG),  Volume 2 Issue 4. 
     * @see http://portal.acm.org/citation.cfm?id=246
     * 
     * @param radius    Radius from origin for circle.
     * @param quadrant  Allow 1 - 4 where 1: x>=0, y>0
     *                                    2: x<0, y>=0
     *                                    3: x<=0, y<0
     *                                    4: x>0, y<=0 
     *                                    
     * @author senseneyj
     */
    
    public static int[][] getQuadrantBoundaryPoints(int radius, int quadrant) {
        ArrayList<int[]> quadrantPoints = new ArrayList<int[]>();
        int x = 0, y = 0, dx = 0, dy = 0;
        if(quadrant == 1 || quadrant == 2) {
            x = radius;
            y = 0;
            dx = -1;
            dy = 1;
        }
        else if(quadrant == 3 || quadrant == 4) {
            x = -radius;
            y = 0;
            dx = 1;
            dy = -1;
        }
        else
            return null;
        int epsilon, epsilonX, epsilonY, epsilonXY;
        while(((quadrant == 1 || quadrant == 2) && x>0) || ((quadrant == 3 || quadrant == 4) && x<0)) {
            int[] temp = new int[2];
            temp[0] = x;
            temp[1] = y;
            quadrantPoints.add(temp);
            epsilon = computeEpsilon(x, y, radius);
            epsilonX = computeEpsilonX(epsilon, x, dx);
            epsilonY = computeEpsilonY(epsilon, y, dy);
            epsilonXY = computeEpsilonXY(epsilon, x, dx, y, dy);
            if(-epsilonXY<epsilonY)
                x = x+dx;
            if(epsilonXY<-epsilonX)
                y = y+dy;
        }
        int[][] quadrantArray = new int[quadrantPoints.size()][2];
        if(quadrant == 1 || quadrant == 3) {
            for(int i=0; i<quadrantPoints.size(); i++) {
                quadrantArray[i] = (int[])quadrantPoints.get(i);
            }
        }
        else if(quadrant == 2 || quadrant == 4) {
            int[] temp = new int[2];
            for(int i=0; i<quadrantPoints.size(); i++) {
                temp = (int[])quadrantPoints.get(i);
                quadrantArray[i][0] = temp[1];
                quadrantArray[i][1] = -temp[0];
            }
        }
        return quadrantArray;
    }
    
    private static int computeEpsilon(int x, int y, int r) {
        return (int)(Math.pow(x, 2) + Math.pow(y, 2) - Math.pow(r, 2));
    }
    
    private static int computeEpsilonX(int epsilon, int x, int dx) {
        return epsilon + 2*x*dx + 1;
    }
    
    private static int computeEpsilonY(int epsilon, int y, int dy) {
        return epsilon + 2*y*dy + 1;
    }
    
    private static int computeEpsilonXY(int epsilon, int x, int dx, int y, int dy) {
       return epsilon + 2*x*dx + 2*y*dy + 2;
    }
}

