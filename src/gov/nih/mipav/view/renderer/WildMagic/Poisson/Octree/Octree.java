package gov.nih.mipav.view.renderer.WildMagic.Poisson.Octree;

import java.util.*;
import java.io.*;

import gov.nih.mipav.view.renderer.WildMagic.Poisson.Geometry.*;
import gov.nih.mipav.view.renderer.WildMagic.Poisson.MarchingCubes.*;

public class Octree {

	public static int DIMENSION = 3;

	NeighborKey neighborKey = new NeighborKey();
	NeighborKey2 neighborKey2 = new NeighborKey2();

	float radius;
	int width;

	public static double maxMemoryUsage = 0;

	public Vector<Point3D> normals;
	public float postNormalSmooth;
	public OctNode tree = new OctNode();
	public FunctionData fData;

	public static double ITERATION_POWER = 1.0 / 3;
	public static int MEMORY_ALLOCATOR_BLOCK_SIZE = 1 << 12;

	public static int READ_SIZE = 1024;

	public static float PAD_SIZE = 1.0f;

	public static float EPSILON = (float) (1e-6);
	public static float ROUND_EPS = (float) (1e-5);
	
	private Geometry geo = new Geometry();

	private int Degree;
	
	public Octree(int _Degree) {
		Degree = _Degree;
		fData = new FunctionData(Degree);
		radius = 0;
		width = 0;
		postNormalSmooth = 0;
	}

	public void setNodeIndices(OctNode node, int[] idx) {
		node.nodeData.nodeIndex = idx[0];
		idx[0]++;
		if (node.children != null) {
			for (int i = 0; i < Cube.CORNERS; i++) {
				setNodeIndices((OctNode) (node.children[i]), idx);
			}
		}
	}

	public int NonLinearSplatOrientedPoint(OctNode node, final Point3D position, final Point3D normal) {
		double x, dxdy, dxdydz;
		double[][] dx = new double[DIMENSION][3];
		int i, j, k;
		Neighbors neighbors = neighborKey.setNeighbors(node);
		double width;
		Point3D center = new Point3D();
		float[] w = new float[1];

		node.centerAndWidth(center, w);
		width = w[0];
		for (i = 0; i < 3; i++) {
			x = (center.coords[i] - position.coords[i] - width) / width;
			dx[i][0] = 1.125d + 1.500d * x + 0.500d * x * x;
			x = (center.coords[i] - position.coords[i]) / width;
			dx[i][1] = 0.750d - x * x;
			dx[i][2] = 1.0d - dx[i][1] - dx[i][0];
			// System.err.println("x = " + x);
			// System.err.println("dx[i][0] = " + dx[i][0] + " dx[i][1] = " + dx[i][1] + " dx[i][2] = " + dx[i][2]);
			// pause();
		}
		for (i = 0; i < 3; i++) {
			for (j = 0; j < 3; j++) {
				dxdy = dx[0][i] * dx[1][j];
				for (k = 0; k < 3; k++) {
					if (neighbors.neighbors[i][j][k] != null) {
						dxdydz = dxdy * dx[2][k];
						int idx = neighbors.neighbors[i][j][k].nodeData.nodeIndex;
						if (idx < 0) {
							Point3D n = new Point3D();
							n.coords[0] = 0;
							n.coords[1] = 0;
							n.coords[2] = 0;
							idx = (int) (normals.size());
							neighbors.neighbors[i][j][k].nodeData.nodeIndex = (int) (normals.size());
							normals.add(n);
						}
						normals.get(idx).coords[0] += (float) (normal.coords[0] * dxdydz);
						normals.get(idx).coords[1] += (float) (normal.coords[1] * dxdydz);
						normals.get(idx).coords[2] += (float) (normal.coords[2] * dxdydz);
						// System.err.println("coords[0] = " +  normals.get(idx).coords[0] + " coords[1] = " + normals.get(idx).coords[1]+ " coords[2] = " + normals.get(idx).coords[2]);
					    // pause();
					}
				}
			}
		}
		return 0;
	}

	public void NonLinearSplatOrientedPoint(final Point3D position, final Point3D normal,
			final int splatDepth, final float samplesPerNode, final int minDepth,
			final int maxDepth) {
		double dx;
		Point3D n = new Point3D();
		OctNode temp;
		int i, cnt = 0;
		double width;
		Point3D myCenter = new Point3D();
		float myWidth;
		myCenter.coords[0] = 0.5f;
		myCenter.coords[1] = 0.5f; 
		myCenter.coords[2] = 0.5f;
		myWidth = 1.0f;

		temp = tree;
		while (temp.depth() < splatDepth) {
			if (temp.children == null) {
				System.err.println("Octree<Degree>::NonLinearSplatOrientedPoint error\n");
				return;
			}
			int cIndex = OctNode.CornerIndex(myCenter, position);
			temp = temp.children[cIndex];
			myWidth /= 2f;
			if ((cIndex & 1) != 0) {
				myCenter.coords[0] += myWidth / 2f;
			} else {
				myCenter.coords[0] -= myWidth / 2f;
			}
			if ((cIndex & 2) != 0) {
				myCenter.coords[1] += myWidth / 2f;
			} else {
				myCenter.coords[1] -= myWidth / 2f;
			}
			if ((cIndex & 4) != 0) {
				myCenter.coords[2] += myWidth / 2f;
			} else {
				myCenter.coords[2] -= myWidth / 2f;
			}
		}
		float[] alpha = new float[1];
		float[] newDepth = new float[1];
		NonLinearGetSampleDepthAndWeight(temp, position, samplesPerNode,
				newDepth, alpha);

		if (newDepth[0] < minDepth) {
			newDepth[0] = (float) (minDepth);
		}
		if (newDepth[0] > maxDepth) {
			newDepth[0] = (float) (maxDepth);
		}
		int topDepth = (int) (Math.ceil(newDepth[0]));

		dx = 1.0 - (topDepth - newDepth[0]);
		if (topDepth <= minDepth) {
			topDepth = minDepth;
			dx = 1;
		} else if (topDepth > maxDepth) {
			topDepth = maxDepth;
			dx = 1;
		}
		while (temp.depth() > topDepth) {
			temp = temp.parent;
		}
		while (temp.depth() < topDepth) {
			if (temp.children == null) {
				temp.initChildren();
			}
			int cIndex = OctNode.CornerIndex(myCenter, position);
			temp = temp.children[cIndex];
			myWidth /= 2f;
			if ((cIndex & 1) != 0) {
				myCenter.coords[0] += myWidth / 2f;
			} else {
				myCenter.coords[0] -= myWidth / 2f;
			}
			if ((cIndex & 2) != 0) {
				myCenter.coords[1] += myWidth / 2f;
			} else {
				myCenter.coords[1] -= myWidth / 2f;
			}
			if ((cIndex & 4) != 0) {
				myCenter.coords[2] += myWidth / 2f;
			} else {
				myCenter.coords[2] -= myWidth / 2f;
			}
		}
		width = 1.0f / (1 << temp.depth());
		for (i = 0; i < DIMENSION; i++) {
			n.coords[i] = normal.coords[i] * alpha[0]
					/ (float) (Math.pow(width, 3)) * (float) (dx);
		}
		NonLinearSplatOrientedPoint(temp, position, n);
		if ((float) Math.abs(1.0f - dx) > EPSILON) {
			dx = (float) (1.0f - dx);
			temp = temp.parent;
			width = 1.0 / (1 << temp.depth());

			for (i = 0; i < DIMENSION; i++) {
				n.coords[i] = normal.coords[i] * alpha[0]
						/ (float) (Math.pow(width, 3)) * (float) (dx);
			}
			NonLinearSplatOrientedPoint(temp, position, n);
		}
	}

	public void NonLinearGetSampleDepthAndWeight(OctNode node,
			final Point3D position, final float samplesPerNode, float[] depth,
			float[] weight) {
		OctNode temp = node;
		weight[0] = (float) (1.0f) / NonLinearGetSampleWeight(temp, position);
		if (weight[0] >= samplesPerNode + 1) {
			depth[0] = (float) (temp.depth() + (float)Math.log(weight[0]
					/ (samplesPerNode + 1))
					/ Math.log((double) (1 << (DIMENSION - 1))));
		} else {
			float oldAlpha, newAlpha;
			oldAlpha = newAlpha = weight[0];
			while (newAlpha < (samplesPerNode + 1) && temp.parent != null) {
				temp = temp.parent;
				oldAlpha = newAlpha;
				newAlpha = (float) (1.0)
						/ NonLinearGetSampleWeight(temp, position);
			}
			depth[0] = (float) (temp.depth() + Math.log(newAlpha
					/ (samplesPerNode + 1))
					/ Math.log(newAlpha / oldAlpha));
		}
		weight[0] = (float) (Math.pow((double) (1 << (DIMENSION - 1)),
				-(double) (depth[0])));
		// System.err.println("depth = " + depth[0] + " weight = " + weight[0]);
		// pause();
	}

	public float NonLinearGetSampleWeight(OctNode node, final Point3D position) {
		float weight = 0f;
		double x, dxdy;
		double[][] dx = new double[DIMENSION][3];
		int i, j, k;
		Neighbors neighbors = neighborKey.setNeighbors(node);
		double width;
		Point3D center = new Point3D();
		float[] w = new float[1];
		node.centerAndWidth(center, w);
		width = w[0];

		for (i = 0; i < DIMENSION; i++) {
			x = (center.coords[i] - position.coords[i] - width) / width;
			dx[i][0] = 1.125d + 1.500d * x + 0.500d * x * x;
			x = (center.coords[i] - position.coords[i]) / width;
			dx[i][1] = 0.750d - x * x;
			dx[i][2] = 1.0d - dx[i][1] - dx[i][0];
		}

		for (i = 0; i < 3; i++) {
			for (j = 0; j < 3; j++) {
				dxdy = dx[0][i] * dx[1][j];
				for (k = 0; k < 3; k++) {
					if (neighbors.neighbors[i][j][k] != null) {
						weight += (float) (dxdy * dx[2][k] * ((TreeNodeData) (neighbors.neighbors[i][j][k].nodeData)).centerWeightContribution);
					}
				}
			}
		}
		return (float) (1.0f / weight);
	}

	public int NonLinearUpdateWeightContribution(OctNode node,
			final Point3D position, final float weight) {
		int i, j, k;
		Neighbors neighbors = neighborKey.setNeighbors(node);
		double x, dxdy;
		double[][] dx = new double[DIMENSION][3];
		double width;
		Point3D center = new Point3D();
		float[] w = new float[1];
		node.centerAndWidth(center, w);
		width = w[0];

		for (i = 0; i < DIMENSION; i++) {
			x = (center.coords[i] - position.coords[i] - width) / width;
			dx[i][0] = 1.125d + 1.500d * x + 0.500d * x * x;
			x = (center.coords[i] - position.coords[i]) / width;
			dx[i][1] = 0.750d - x * x;
			dx[i][2] = 1.0d - dx[i][1] - dx[i][0];
		}

		for (i = 0; i < 3; i++) {
			for (j = 0; j < 3; j++) {
				dxdy = dx[0][i] * dx[1][j] * weight;
				for (k = 0; k < 3; k++) {
					if (neighbors.neighbors[i][j][k] != null) {
						((TreeNodeData) (neighbors.neighbors[i][j][k].nodeData)).centerWeightContribution += (float) (dxdy * dx[2][k]);
						// System.err.println(((TreeNodeData) (neighbors.neighbors[i][j][k].nodeData)).centerWeightContribution);
					    // pause();
					}
				}
			}
		}
		return 0;
	}
	
	  /**
	  * Pauses the display until the user hits enter.
	  */
	  public static void pause() {
	    int count = 0;
	    
	    try {
	      // eat any pending characters
	      for ( int av=System.in.available(); av>0; av-- ) {
	        System.in.read();
	      }
	      System.in.read();// wait for user to hit Enter, discard result
	    } catch ( IOException e ) {
	      System.err.println( "keyboard failed: "+e);
	    }

	  } 
	 public String readLine(InputStream in) throws IOException
	  {
	    StringBuffer buf = new StringBuffer();
	    int c;
	    while ((c = in.read()) > -1 && c != '\n')
	      {
	        buf.append((char) c);
	      }
	    return buf.toString();
	  }

	public int setTree(String fileName, final int maxDepth, final int binary,
			final int kernelDepth, final float samplesPerNode, final float scaleFactor,
			Point3D center, float[] scale, final int resetSamples, final int useConfidence) {

		Point3D min = new Point3D();
		Point3D max = new Point3D();
		Point3D position = new Point3D();
		Point3D normal = new Point3D();
		Point3D myCenter = new Point3D();
		float myWidth;
		int i, cnt = 0;
		boolean readHeader = true;
		int iVertexCount = 0, iTriangleCount;
		OctNode temp;
		int splatDepth = 0;
		// FILE* fp;
		float[] c = new float[2 * DIMENSION];

		TreeNodeData.UseIndex = 1;
		neighborKey.set(maxDepth);
		splatDepth = kernelDepth;
		if (splatDepth < 0) {
			splatDepth = 0;
		}
		// if(binary == 1){fp=fopen(fileName,"rb");}
		// else{fp=fopen(fileName,"r");}
		// if(!fp){return 0;}

		
		System.err.println("Setting bounding box\n");
	
		File f = new File(fileName);
		try {
			DataInputStream in = new DataInputStream(new BufferedInputStream(new FileInputStream(f)));
			String s, token;
			// Read through once to get the center and scale
			
			while ( (s = readLine(in)).length() > 0) {
                StringTokenizer st = new StringTokenizer(s);
                while ( st.hasMoreTokens() && readHeader ) {
                    // System.err.print(st.nextToken() + " ");
                    token = st.nextToken();
                    if ( token.equals("vertex")) {
                        iVertexCount = Integer.valueOf(st.nextToken());
                    } else if ( token.equals("face") ) {
                        iTriangleCount = Integer.valueOf(st.nextToken());
                        readLine(in);
                        readLine(in);  // skip two lines follow the face count attribute in PLY file format.
                        readHeader = false;
                        break;
                    }
                }
                if ( readHeader == false) break;
            }
			
			while ((s = readLine(in)).length() > 0 ) {
				// if(binary){if(fread(c,sizeof(float),2*DIMENSION,fp)!=6){break;}}
				// else{if(fscanf(fp," %f %f %f %f %f %f
				// ",&c[0],&c[1],&c[2],&c[3],&c[4],&c[5])!=2*DIMENSION){break;}}
				if ( cnt == iVertexCount) break;
				StringTokenizer st = new StringTokenizer(s);
				
				while ( st.hasMoreTokens() ) {
					c[0] = Float.valueOf(st.nextToken());
			    	c[1] = Float.valueOf(st.nextToken());
			    	c[2] = Float.valueOf(st.nextToken());
			    	c[3] = Float.valueOf(st.nextToken());
			    	c[4] = Float.valueOf(st.nextToken());
			    	c[5] = Float.valueOf(st.nextToken());
				}
				
				for (i = 0; i < DIMENSION; i++) {
					if (cnt == 0 || c[i] < min.coords[i]) {
						min.coords[i] = c[i];
					}
					if (cnt == 0 || c[i] > max.coords[i]) {
						max.coords[i] = c[i];
					}
				}
				cnt++;
			}
	
			for (i = 0; i < DIMENSION; i++) {
				if (i == 0 || scale[0] < max.coords[i] - min.coords[i]) {
					scale[0] = (float) (max.coords[i] - min.coords[i]);
				}
				center.coords[i] = (float) (max.coords[i] + min.coords[i]) / 2f;
			}
			System.err.println("Samples: " + cnt);
			scale[0] *= scaleFactor;
			for (i = 0; i < DIMENSION; i++) {
				center.coords[i] -= scale[0] / 2f;
			}
			
		
			if (splatDepth > 0) {
				System.err.println("Setting sample weights\n");
				cnt = 0;
				readHeader = true;
				in = new DataInputStream(new BufferedInputStream(new FileInputStream(f)));
				while ( (s = readLine(in)).length() > 0) {
	                StringTokenizer st = new StringTokenizer(s);
	                while ( st.hasMoreTokens() && readHeader ) {
	                    // System.err.print(st.nextToken() + " ");
	                    token = st.nextToken();
	                    if ( token.equals("vertex")) {
	                        iVertexCount = Integer.valueOf(st.nextToken());
	                    } else if ( token.equals("face") ) {
	                        iTriangleCount = Integer.valueOf(st.nextToken());
	                        readLine(in);
	                        readLine(in);  // skip two lines follow the face count attribute in PLY file format.
	                        readHeader = false;
	                        break;
	                    }
	                }
	                if ( readHeader == false) break;
	            }
				
				// fseek(fp,SEEK_SET,0);
				
				while ((s = readLine(in)).length() > 0) {
					// if(binary){if(fread(c,sizeof(float),2*DIMENSION,fp)!=2*DIMENSION){break;}}
					// else{if(fscanf(fp," %f %f %f %f %f %f
					// ",&c[0],&c[1],&c[2],&c[3],&c[4],&c[5])!=2*DIMENSION){break;}}
					if ( cnt == iVertexCount) break;
					StringTokenizer st = new StringTokenizer(s);
					
					while ( st.hasMoreTokens() ) {
						c[0] = Float.valueOf(st.nextToken());
				    	c[1] = Float.valueOf(st.nextToken());
				    	c[2] = Float.valueOf(st.nextToken());
				    	c[3] = Float.valueOf(st.nextToken());
				    	c[4] = Float.valueOf(st.nextToken());
				    	c[5] = Float.valueOf(st.nextToken());
					}
					
					for (i = 0; i < DIMENSION; i++) {
						position.coords[i] = (c[i] - center.coords[i]) / scale[0];
						normal.coords[i] = c[DIMENSION + i];
					}
					myCenter.coords[0] = myCenter.coords[1] = myCenter.coords[2] = 0.5f;
					myWidth = 1.0f;
					for (i = 0; i < DIMENSION; i++) {
						if (position.coords[i] < myCenter.coords[i] - myWidth / 2f
								|| position.coords[i] > myCenter.coords[i]
										+ myWidth / 2f) {
							break;
						}
					}
					if (i != DIMENSION) {
						continue;
					}
					temp = tree;
					int d = 0;
					float weight = (1.0f);
					if (useConfidence == 1) {
						weight = (float) (geo.Length(normal));
					}
					while (d < splatDepth) {
						NonLinearUpdateWeightContribution(temp, position, weight);
						if (temp.children == null) {
							temp.initChildren();
						}
						int cIndex = OctNode.CornerIndex(myCenter, position);
						temp = temp.children[cIndex];
						myWidth /= 2f;
						if ((cIndex & 1) != 0) {
							myCenter.coords[0] += myWidth / 2f;
						} else {
							myCenter.coords[0] -= myWidth / 2f;
						}
						if ((cIndex & 2) != 0) {
							myCenter.coords[1] += myWidth / 2f;
						} else {
							myCenter.coords[1] -= myWidth / 2f;
						}
						if ((cIndex & 4) != 0) {
							myCenter.coords[2] += myWidth / 2f;
						} else {
							myCenter.coords[2] -= myWidth / 2f;
						}
						d++;
					}
					NonLinearUpdateWeightContribution(temp, position, weight);
					cnt++;
				}
			}
	
			System.err.println("Adding Points and Normals\n");
			normals = new Vector<Point3D>();
			cnt = 0;
			readHeader = true;
			// fseek(fp,SEEK_SET,0);
			in = new DataInputStream(new BufferedInputStream(new FileInputStream(f)));
			
			while ( (s = readLine(in)).length() > 0) {
                StringTokenizer st = new StringTokenizer(s);
                while ( st.hasMoreTokens() && readHeader ) {
                    // System.err.print(st.nextToken() + " ");
                    token = st.nextToken();
                    if ( token.equals("vertex")) {
                        iVertexCount = Integer.valueOf(st.nextToken());
                    } else if ( token.equals("face") ) {
                        iTriangleCount = Integer.valueOf(st.nextToken());
                        readLine(in);
                        readLine(in);  // skip two lines follow the face count attribute in PLY file format.
                        readHeader = false;
                        break;
                    }
                }
                if ( readHeader == false) break;
            }
			
			while ((s = readLine(in)).length() > 0) {
				// if(binary){if(fread(c,sizeof(float),2*DIMENSION,fp)!=2*DIMENSION){break;}}
				// else{if(fscanf(fp," %f %f %f %f %f %f
				// ",&c[0],&c[1],&c[2],&c[3],&c[4],&c[5])!=2*DIMENSION){break;}}
				if ( cnt == iVertexCount) break;
				StringTokenizer st = new StringTokenizer(s);
				
				while ( st.hasMoreTokens() ) {
					c[0] = Float.valueOf(st.nextToken());
			    	c[1] = Float.valueOf(st.nextToken());
			    	c[2] = Float.valueOf(st.nextToken());
			    	c[3] = Float.valueOf(st.nextToken());
			    	c[4] = Float.valueOf(st.nextToken());
			    	c[5] = Float.valueOf(st.nextToken());
				}
				cnt++;
				for (i = 0; i < DIMENSION; i++) {
					position.coords[i] = (c[i] - center.coords[i]) / scale[0];
					normal.coords[i] = c[DIMENSION + i];
				}
				myCenter.coords[0] = myCenter.coords[1] = myCenter.coords[2] = 0.5f;
				myWidth = 1.0f;
				for (i = 0; i < DIMENSION; i++) {
					if (position.coords[i] < myCenter.coords[i] - myWidth / 2f
							|| position.coords[i] > myCenter.coords[i] + myWidth
									/ 2f) {
						break;
					}
				}
				if (i != DIMENSION) {
					continue;
				}
				float l = (float) (geo.Length(normal));
				if (l < EPSILON) {
					continue;
				}
				if (useConfidence == 0) {
					normal.coords[0] /= l;
					normal.coords[1] /= l;
					normal.coords[2] /= l;
				}
				l = (float) (2 << maxDepth);
				normal.coords[0] *= l;
				normal.coords[1] *= l;
				normal.coords[2] *= l;
	
				if (resetSamples == 1 && samplesPerNode > 0 && splatDepth != 0) {   // Ruida, == 1 or != 0
					NonLinearSplatOrientedPoint(position, normal, splatDepth,
							samplesPerNode, 1, maxDepth);
				} else {
					float alpha = 1;
					temp = tree;
					if (splatDepth != 0) {
						int d = 0;
						while (d < splatDepth) {
							int cIndex = OctNode.CornerIndex(myCenter, position);
							temp = temp.children[cIndex];
							myWidth /= 2f;
							if ((cIndex & 1) != 0) {
								myCenter.coords[0] += myWidth / 2f;
							} else {
								myCenter.coords[0] -= myWidth / 2f;
							}
							if ((cIndex & 2) != 0) {
								myCenter.coords[1] += myWidth / 2f;
							} else {
								myCenter.coords[1] -= myWidth / 2f;
							}
							if ((cIndex & 4) != 0) {
								myCenter.coords[2] += myWidth / 2f;
							} else {
								myCenter.coords[2] -= myWidth / 2f;
							}
							d++;
						}
						alpha = NonLinearGetSampleWeight(temp, position);
					}
					for (i = 0; i < DIMENSION; i++) {
						normal.coords[i] *= alpha;
					}
					int d = 0;
					while (d < maxDepth) {
						if (temp.children == null) {
							temp.initChildren();
						}
						int cIndex = OctNode.CornerIndex(myCenter, position);
						temp = temp.children[cIndex];
						myWidth /= 2f;
						if ((cIndex & 1) != 0) {
							myCenter.coords[0] += myWidth / 2f;
						} else {
							myCenter.coords[0] -= myWidth / 2f;
						}
						if ((cIndex & 2) != 0) {
							myCenter.coords[1] += myWidth / 2f;
						} else {
							myCenter.coords[1] -= myWidth / 2f;
						}
						if ((cIndex & 4) != 0) {
							myCenter.coords[2] += myWidth / 2f;
						} else {
							myCenter.coords[2] -= myWidth / 2f;
						}
						d++;
					}
					NonLinearSplatOrientedPoint(temp, position, normal);
				}
			}
			// DumpOutput("Memory Usage: %.3f MB\n",float(MemoryUsage()));
			// fclose(fp);
			in.close();
		} catch (FileNotFoundException e) {
			System.err.println("ERROR: Can't find file " + f.getName());
			return 0;
		} catch (IOException e) {
			return 0;
		}	
		return cnt;
	}

	public void setFunctionData(final PPolynomial ReconstructionFunction,
			final int maxDepth, final int normalize, final float normalSmooth) {

		radius = (float) (Math.abs(ReconstructionFunction.polys[0].start));
		width = (int) ((double) (radius + 0.5 - EPSILON) * 2);
		System.err.println("radius = " + radius + " width = " + width);
		if (normalSmooth > 0) {
			postNormalSmooth = normalSmooth;
		}
		fData.set(maxDepth, ReconstructionFunction, normalize, 1);
	}

	public void finalize1(final int refineNeighbors)
	{
		OctNode temp;

		if(refineNeighbors>=0){
			RefineFunction rf = new RefineFunction();
			temp=tree.nextNode(null);
			while(temp != null ){
				if(temp.nodeData.nodeIndex>=0 && geo.Length(normals.get(temp.nodeData.nodeIndex))>EPSILON){
					rf.depth=temp.depth()-refineNeighbors;
					OctNode.ProcessMaxDepthNodeAdjacentNodes(fData.depth,temp,2*width, tree,1,temp.depth()-refineNeighbors,rf, 1);
				}
				temp=tree.nextNode(temp);
			}
		}
		else if(refineNeighbors==-1234){
			temp=tree.nextLeaf(null);
			while(temp != null ){
				if( temp.children== null && temp.depth()<fData.depth){temp.initChildren();}
				temp=tree.nextLeaf(temp);
			}
		}
	}

	public void finalize2(final int refineNeighbors)
	{
		OctNode temp;

		if(refineNeighbors>=0){
			RefineFunction rf = new RefineFunction();
			temp=tree.nextNode(null);
			while(temp != null ){
				if((float)Math.abs(temp.nodeData.value)>EPSILON){
					rf.depth=temp.depth()-refineNeighbors;
					OctNode.ProcessMaxDepthNodeAdjacentNodes(fData.depth,temp,2*width,tree,1,temp.depth()-refineNeighbors,rf, 1);
				}
				temp=tree.nextNode(temp);
			}
		}
	}

	public final float GetDivergence(final int idx[], final Point3D normal) {
		double dot = fData.dotTable[idx[0]] * fData.dotTable[idx[1]]
				* fData.dotTable[idx[2]];
		return (float) (dot * (fData.dDotTable[idx[0]] * normal.coords[0]
				+ fData.dDotTable[idx[1]] * normal.coords[1] + fData.dDotTable[idx[2]]
				* normal.coords[2]));
	}

	public final float GetLaplacian(final int idx[]) {
		return (float) (fData.dotTable[idx[0]] * fData.dotTable[idx[1]]
				* fData.dotTable[idx[2]] * (fData.d2DotTable[idx[0]]
				+ fData.d2DotTable[idx[1]] + fData.d2DotTable[idx[2]]));
	}

	public final float GetDotProduct(final int idx[]) {
		return (float) (fData.dotTable[idx[0]] * fData.dotTable[idx[1]] * fData.dotTable[idx[2]]);
	}

	public int GetFixedDepthLaplacian(SparseSymmetricMatrix matrix, final int depth,
			final SortedTreeNodes sNodes) {
		LaplacianMatrixFunction mf = new LaplacianMatrixFunction();
		mf.ot = this;
		mf.offset = sNodes.nodeCount[depth];
		
		matrix.Resize(sNodes.nodeCount[depth + 1] - sNodes.nodeCount[depth]);
		// mf.rowElements=(MatrixEntry<float>*)malloc(sizeof(MatrixEntry<float>)*matrix.rows);
	    // System.err.println("rumor = " +(sNodes.nodeCount[depth+1]-sNodes.nodeCount[depth]) + " mf.offset = " + mf.offset);
		mf.rowElements = new MatrixEntry[matrix.rows];
		
		for ( int j = 0; j < matrix.rows; j++ )
			mf.rowElements[j] = new MatrixEntry();
		
		for (int i = sNodes.nodeCount[depth]; i < sNodes.nodeCount[depth + 1]; i++) {
			mf.elementCount = 0;
			mf.d2 = (int)(sNodes.treeNodes[i].d[0]);
			mf.x2 = (int)(sNodes.treeNodes[i].off[0]);
			mf.y2 = (int)(sNodes.treeNodes[i].off[1]);
			mf.z2 = (int)(sNodes.treeNodes[i].off[2]);
			mf.index[0] = mf.x2;
			mf.index[1] = mf.y2;
			mf.index[2] = mf.z2;
			OctNode.ProcessTerminatingNodeAdjacentNodes(fData.depth, sNodes.treeNodes[i], 2 * width - 1, tree, 1, mf, 1);
			matrix.SetRowSize(i - sNodes.nodeCount[depth], mf.elementCount);
			// memcpy(matrix.m_ppElements[i-sNodes.nodeCount[depth]],mf.rowElements,sizeof(MatrixEntry<float>)*mf.elementCount);
			// System.err.println("(i - sNodes.nodeCount[depth]) = " + (i - sNodes.nodeCount[depth]));
			// System.arraycopy(mf.rowElements, 0, matrix.m_ppElements[i - sNodes.nodeCount[depth]], 0, mf.elementCount);
		    for ( int k = 0; k < mf.elementCount; k++ ) {
		    	matrix.m_ppElements[i - sNodes.nodeCount[depth]][k].N = mf.rowElements[k].N;
		    	matrix.m_ppElements[i - sNodes.nodeCount[depth]][k].Value = mf.rowElements[k].Value;
		    }
			// matrix.print();
			// mf.print();
			// Octree.pause();
		}
		mf.rowElements = null;
		return 1;
	}

	public int GetRestrictedFixedDepthLaplacian(SparseSymmetricMatrix matrix,
			final int depth, final int[] entries, final int entryCount, final OctNode rNode,
			final float radius, final SortedTreeNodes sNodes) {
		int i;
		int[] dep = new int[1];
		RestrictedLaplacianMatrixFunction mf = new RestrictedLaplacianMatrixFunction();
		float myRadius = (int) (2 * radius - ROUND_EPS) + ROUND_EPS;
		mf.ot = this;
		mf.radius = radius;
		rNode.depthAndOffset(dep, mf.offset);
		mf.depth = dep[0];
		matrix.Resize(entryCount);
		// mf.rowElements=(MatrixEntry<float>*)malloc(sizeof(MatrixEntry<float>)*matrix.rows);
		mf.rowElements = new MatrixEntry[matrix.rows];
		for ( int j = 0; j < matrix.rows; j++ )
			mf.rowElements[j] = new MatrixEntry();
		for (i = 0; i < entryCount; i++) {
			sNodes.treeNodes[entries[i]].nodeData.nodeIndex = i;
		}
		for (i = 0; i < entryCount; i++) {
			mf.elementCount = 0;
			mf.index[0] = (int)(sNodes.treeNodes[entries[i]].off[0]);
			mf.index[1] = (int)(sNodes.treeNodes[entries[i]].off[1]);
			mf.index[2] = (int)(sNodes.treeNodes[entries[i]].off[2]);
			OctNode.ProcessTerminatingNodeAdjacentNodes(fData.depth,
					sNodes.treeNodes[entries[i]], 2 * width - 1, tree, 1, mf, 1);
			matrix.SetRowSize(i, mf.elementCount);
			// memcpy(matrix.m_ppElements[i],mf.rowElements,sizeof(MatrixEntry<float>)*mf.elementCount);
			// System.arraycopy(mf.rowElements, 0, matrix.m_ppElements[i], 0, mf.elementCount);
			for ( int k = 0; k < mf.elementCount; k++ ) {
				 matrix.m_ppElements[i][k].N = mf.rowElements[k].N;
				 matrix.m_ppElements[i][k].Value = mf.rowElements[k].Value;
			}
		}
		for (i = 0; i < entryCount; i++) {
			sNodes.treeNodes[entries[i]].nodeData.nodeIndex = (int)entries[i];
			
		}
		mf.rowElements = null;
		return 1;
	}

	public int LaplacianMatrixIteration(final int subdivideDepth) {
		int i, iter = 0;
		SortedTreeNodes sNodes = new SortedTreeNodes();
		double t;
		fData.setDotTables(fData.D2_DOT_FLAG);
		
		sNodes.set(tree, 1);

		/*
		for (int j = 0; j < sNodes.maxDepth; j++ ) {
			System.err.println("Depth: " + j);
			for(i=sNodes.nodeCount[j];i<sNodes.nodeCount[j+1];i++){
				System.err.println("(float)sNodes.treeNodes[" + i + "].nodeData.value = " + (float)sNodes.treeNodes[i].nodeData.value);
				
			}
			pause();
		}
	    */
		
		SparseMatrix.SetAllocator(MEMORY_ALLOCATOR_BLOCK_SIZE);

		sNodes.treeNodes[0].nodeData.value = 0;
		for (i = 1; i < sNodes.maxDepth; i++) {
			System.err.println("Depth:  " + i + "/" + (sNodes.maxDepth - 1));
			t = System.currentTimeMillis();
			if (subdivideDepth > 0) {
				iter += SolveFixedDepthMatrix(i, subdivideDepth, sNodes);
				/*
				for (int j = 0; j < sNodes.maxDepth; j++ ) {
					System.err.println("Depth: " + j);
					for(int k=sNodes.nodeCount[j];k<sNodes.nodeCount[j+1];k++){
						System.err.println("(float)sNodes.treeNodes[" + k + "].nodeData.value = " + (float)sNodes.treeNodes[k].nodeData.value);
						
					}
					pause();
				}
				*/
			} else {
				iter += SolveFixedDepthMatrix(i, sNodes);
			}
		}
		// SparseMatrix<float>::Allocator.reset();
		SparseMatrix.Allocator.reset();
		fData.clearDotTables(fData.DOT_FLAG | fData.D_DOT_FLAG
				| fData.D2_DOT_FLAG);
		return iter;
	}

	public int SolveFixedDepthMatrix(final int depth, final SortedTreeNodes sNodes){
		int i,iter=0;
		VectorD V = new VectorD();
		VectorD Solution = new VectorD();
		final SparseSymmetricMatrix matrix = new SparseSymmetricMatrix();
		float myRadius;
		double gTime,sTime,uTime;
		float dx,dy,dz;
		int x1,x2,y1,y2,z1,z2;
		Vector<Float> Diagonal = new Vector<Float>();

		gTime=System.currentTimeMillis();
	    /*
		for (int j = 0; j < sNodes.maxDepth; j++ ) {
			System.err.println("Depth: " + j);
			for(i=sNodes.nodeCount[j];i<sNodes.nodeCount[j+1];i++){
				System.err.println("(float)sNodes.treeNodes[" + i + "].nodeData.value = " + (float)sNodes.treeNodes[i].nodeData.value);
				
			}
			pause();
		}
        */
		
		V.Resize(sNodes.nodeCount[depth+1]-sNodes.nodeCount[depth]);
		for(i=sNodes.nodeCount[depth];i<sNodes.nodeCount[depth+1];i++){
			// System.err.println("loop");
			// System.err.println("Before: (float)sNodes.treeNodes[" + i + "].nodeData.value = " + (float)sNodes.treeNodes[i].nodeData.value);
			// System.err.println("i = " + i + "  depth = " + depth + "  sNodes.nodeCount[depth] = " + sNodes.nodeCount[depth] + " sNodes.nodeCount[depth+1] = " + sNodes.nodeCount[depth+1]);
			// System.err.println("(float)sNodes.treeNodes[" + i + "].nodeData.value = " + (float)sNodes.treeNodes[i].nodeData.value);
			// Float ff = new Float(sNodes.treeNodes[i].nodeData.value);
			// V.m_pV[i-sNodes.nodeCount[depth]] =  ff.floatValue(); // (float)(sNodes.treeNodes[i].nodeData.value); // sNodes.treeNodes[i].nodeData.value;
			V.m_pV[i-sNodes.nodeCount[depth]] = sNodes.treeNodes[i].nodeData.value;
			// System.err.println("i = " + i + "  depth = " + depth + "  sNodes.nodeCount[depth] = " + sNodes.nodeCount[depth] + " sNodes.nodeCount[depth+1] = " + sNodes.nodeCount[depth+1]);
			// System.err.println("(float)sNodes.treeNodes[" + i + "].nodeData.value = " + (float)sNodes.treeNodes[i].nodeData.value);
		}
	    // pause();
		// SparseSymmetricMatrix ssm = new SparseSymmetricMatrix();
		
		SparseSymmetricMatrix.Allocator.rollBack();
		GetFixedDepthLaplacian(matrix,depth,sNodes);
		
		
		
	    // matrix.print();
	    // pause();
		gTime=System.currentTimeMillis()-gTime;
		System.err.println("\tMatrix entries: " + matrix.Entries() + " / " + matrix.rows + "^2 = "+ 100.0*(matrix.Entries()/(double)(matrix.rows)/matrix.rows) + "%");
		// DumpOutput("\tMemory Usage: %.3f MB\n",float(MemoryUsage()));
		sTime=System.currentTimeMillis();
		iter+=SparseSymmetricMatrix.Solve(matrix,V,(int)(Math.pow(matrix.rows,ITERATION_POWER)),Solution,(double)(EPSILON),1);
		sTime=System.currentTimeMillis()-sTime;
		uTime=System.currentTimeMillis();
		
		
		for(i=sNodes.nodeCount[depth];i<sNodes.nodeCount[depth+1];i++){
			// Float ff = new Float(Solution.get(i-sNodes.nodeCount[depth]));
			// sNodes.treeNodes[i].nodeData.value= ff.floatValue();
			sNodes.treeNodes[i].nodeData.value= (float)Solution.get(i-sNodes.nodeCount[depth]);
		    // System.err.println("after: sNodes.treeNodes[" + i + " ].nodeData.value = " + sNodes.treeNodes[i].nodeData.value);
			
		}
		
		// pause();
		
		myRadius=(float)(radius+ROUND_EPS-0.5f);
		myRadius /=(1<<depth);

		if(depth<sNodes.maxDepth-1){
			LaplacianProjectionFunction pf = new LaplacianProjectionFunction();
			OctNode node1, node2;
			pf.ot=this;
			int idx1,idx2,offf=sNodes.nodeCount[depth];
			// First pass: idx2 is the solution coefficient propogated
			for(i=0;i<matrix.rows;i++){
				idx1=i;
				node1=sNodes.treeNodes[idx1+offf];
				if(node1.children == null ){continue;}
				x1= (int)(node1.off[0]);
				y1= (int)(node1.off[1]);
				z1= (int)(node1.off[2]);
				for(int j=0;j<matrix.rowSizes[i];j++){
					idx2=matrix.m_ppElements[i][j].N;
					node2=sNodes.treeNodes[idx2+offf];
					x2= (int)(node2.off[0]);
					y2= (int)(node2.off[1]);
					z2= (int)(node2.off[2]);
					pf.value=Solution.get(idx2);
					pf.index[0]=x2;
					pf.index[1]=y2;
					pf.index[2]=z2;
					dx=(float)(x2-x1)/(1<<depth);
					dy=(float)(y2-y1)/(1<<depth);
					dz=(float)(z2-z1)/(1<<depth);
					if((float)Math.abs(dx)<myRadius && (float)Math.abs(dy)<myRadius && (float)Math.abs(dz)<myRadius){
						node1.processNodeNodes(node2,pf,0);}
					else{OctNode.ProcessNodeAdjacentNodes(fData.depth,node2,width,node1,width,pf,0);}
				}
			}
			// Second pass: idx1 is the solution coefficient propogated
			for(i=0;i<matrix.rows;i++){
				idx1=i;
				node1=sNodes.treeNodes[idx1+offf];
				x1= (int)(node1.off[0]);
				y1= (int)(node1.off[1]);
				z1= (int)(node1.off[2]);
				pf.value=Solution.get(idx1);
				pf.index[0]=x1;
				pf.index[1]=y1;
				pf.index[2]=z1;
				for(int j=0;j<matrix.rowSizes[i];j++){
					idx2=matrix.m_ppElements[i][j].N;
					node2=sNodes.treeNodes[idx2+offf];
					if(idx1!=idx2 && node2.children != null ){
						x2= (int)(node2.off[0]);
						y2= (int)(node2.off[1]);
						z2= (int)(node2.off[2]);
						dx=(float)(x1-x2)/(1<<depth);
						dy=(float)(y1-y2)/(1<<depth);
						dz=(float)(z1-z2)/(1<<depth);
						if((float)Math.abs(dx)<myRadius && (float)Math.abs(dy)<myRadius && (float)Math.abs(dz)<myRadius){
							node2.processNodeNodes(node1,pf,0);}
						else{OctNode.ProcessNodeAdjacentNodes(fData.depth,node1,width,node2,width,pf,0);}
					}
				}
			}
		}
		uTime=System.currentTimeMillis()-uTime;
		System.err.println("\tGot / Solved / Updated in: " + gTime + " / " + sTime + " / " + uTime);
	    
		return iter;
	}	
	
	public int SolveFixedDepthMatrix(final int depth, final int startingDepth, final SortedTreeNodes sNodes){
		int i,j,d,iter=0;
		SparseSymmetricMatrix matrix = new SparseSymmetricMatrix();
		AdjacencySetFunction asf = new AdjacencySetFunction();
		AdjacencyCountFunction acf = new AdjacencyCountFunction();
		VectorD Values = new VectorD();
		
		VectorD SubValues = new VectorD();
		VectorD SubSolution = new VectorD();
		double gTime,sTime,uTime;
		float myRadius,myRadius2;
		float dx,dy,dz;
		VectorF Diagonal = new VectorF();
       
		
		if(startingDepth>=depth){
			return SolveFixedDepthMatrix(depth,sNodes);
		}
        System.err.println("***********************************************************************************");
		Values.Resize(sNodes.nodeCount[depth+1]-sNodes.nodeCount[depth]);
        /*
		for (j = 0; j < sNodes.maxDepth; j++ ) {
			System.err.println("Depth: " + j);
			for(i=sNodes.nodeCount[j];i<sNodes.nodeCount[j+1];i++){
				System.err.println("(float)sNodes.treeNodes[" + i + "].nodeData.value = " + (float)sNodes.treeNodes[i].nodeData.value);	
			}
			pause();
		}
		*/
		for(i=sNodes.nodeCount[depth];i<sNodes.nodeCount[depth+1];i++){
			// Float ff = new Float(sNodes.treeNodes[i].nodeData.value);
			// Values.set(i-sNodes.nodeCount[depth], sNodes.treeNodes[i].nodeData.value);
			// Values.m_pV[i-sNodes.nodeCount[depth]] = ff.floatValue();
			Values.m_pV[i-sNodes.nodeCount[depth]] = sNodes.treeNodes[i].nodeData.value;
			// System.err.println("(float)sNodes.treeNodes[" + i + "].nodeData.value = " + (float)sNodes.treeNodes[i].nodeData.value);	
			
		    sNodes.treeNodes[i].nodeData.value=0;
			// System.err.println("Values.m_pV"+ (i-sNodes.nodeCount[depth]) + "] = " + Values.m_pV[i-sNodes.nodeCount[depth]]);
			
		}
        // pause();
		
		myRadius=2f*radius-0.5f;
		myRadius=(int)(myRadius-ROUND_EPS)+ROUND_EPS;
		myRadius2=(float)(radius+ROUND_EPS-0.5f);
		d=depth-startingDepth;
		for(i=sNodes.nodeCount[d];i<sNodes.nodeCount[d+1];i++){
			gTime=System.currentTimeMillis();
			OctNode temp;
			// Get all of the entries associated to the subspace
			acf.adjacencyCount=0;
			temp=sNodes.treeNodes[i].nextNode(null);
			while(temp != null){
				if(temp.depth()==depth){
					acf.Function(temp,temp);
					temp=sNodes.treeNodes[i].nextBranch(temp);
				}
				else{temp=sNodes.treeNodes[i].nextNode(temp);}
			}
			for(j=sNodes.nodeCount[d];j<sNodes.nodeCount[d+1];j++){
				if(i==j){continue;}
				OctNode.ProcessFixedDepthNodeAdjacentNodes(fData.depth,sNodes.treeNodes[i],1,sNodes.treeNodes[j],2*width-1,depth,acf, 1);
			}
			if(acf.adjacencyCount == 0 ){continue;}
			asf.adjacencies=new int[acf.adjacencyCount];
			asf.adjacencyCount=0;
			temp=sNodes.treeNodes[i].nextNode(null);
			while(temp != null){
				if(temp.depth()==depth){
					asf.Function(temp,temp);
					temp=sNodes.treeNodes[i].nextBranch(temp);
				}
				else{temp=sNodes.treeNodes[i].nextNode(temp);}
			}
			for(j=sNodes.nodeCount[d];j<sNodes.nodeCount[d+1];j++){
				if(i==j){continue;}
				OctNode.ProcessFixedDepthNodeAdjacentNodes(fData.depth,sNodes.treeNodes[i],1,sNodes.treeNodes[j],2*width-1,depth,asf, 1);
			}
           
			System.err.println("\tNodes[ " + (i-sNodes.nodeCount[d]+1) + " / " + (sNodes.nodeCount[d+1]-sNodes.nodeCount[d]) + " ]: " + asf.adjacencyCount);
			
			// Get the associated vector
			SubValues.Resize(asf.adjacencyCount);
			
			for(j=0;j<asf.adjacencyCount;j++){
				// Float ff = new Float(Values.get(asf.adjacencies[j]-sNodes.nodeCount[depth]));
				// SubValues.set(j, Values.get(asf.adjacencies[j]-sNodes.nodeCount[depth]));
				// SubValues.m_pV[j] = ff.floatValue();
				SubValues.m_pV[j] = Values.get(asf.adjacencies[j]-sNodes.nodeCount[depth]);
			}
			// SubValues.print();
			// pause();
			SubSolution.Resize(asf.adjacencyCount);
			for(j=0;j<asf.adjacencyCount;j++){
				// Float ff = new Float(sNodes.treeNodes[asf.adjacencies[j]].nodeData.value);
				// SubSolution.set(j, sNodes.treeNodes[asf.adjacencies[j]].nodeData.value);
				// SubSolution.m_pV[j] = ff.floatValue();
				SubSolution.m_pV[j] = sNodes.treeNodes[asf.adjacencies[j]].nodeData.value;
			}
			// SubSolution.print();
			// pause();
		    
			// Get the associated matrix
			// SparseSymmetricMatrix ssm = new SparseSymmetricMatrix();
		
			SparseSymmetricMatrix.Allocator.rollBack();
			GetRestrictedFixedDepthLaplacian(matrix,depth,asf.adjacencies,asf.adjacencyCount,sNodes.treeNodes[i],myRadius,sNodes);
			
		    	
			gTime=( System.currentTimeMillis()-gTime) / 1000f;
			System.err.println("\t\tMatrix entries: " + matrix.Entries() + " / " + matrix.rows + " ^2 = " + (100.0*(matrix.Entries()/(double)(matrix.rows))/matrix.rows));
			// DumpOutput("\t\tMemory Usage: %.3f MB\n",float(MemoryUsage()));

		
			// Solve the matrix
			sTime=System.currentTimeMillis();
			
			
			/*
			System.err.println("SubSolution before:");
			matrix.print();
			System.err.println("SubSolution before:");
			pause();
			*/
		
			iter+=SparseSymmetricMatrix.Solve(matrix,SubValues,(int)(Math.pow(matrix.rows,ITERATION_POWER)),SubSolution,(double)(EPSILON),0, true);
			
			sTime=( System.currentTimeMillis()-sTime ) / 1000f;
            /*
			System.err.println("SubSolution after:");
			SubSolution.print();
			System.err.println("SubSolution after:");
			pause();
			*/
			uTime=System.currentTimeMillis();
			LaplacianProjectionFunction lpf = new LaplacianProjectionFunction();
			lpf.ot=this;
            
			// Update the solution for all nodes in the sub-tree
			for(j=0;j<asf.adjacencyCount;j++){
				temp=sNodes.treeNodes[asf.adjacencies[j]];
				while(temp.depth()>sNodes.treeNodes[i].depth()){temp=temp.parent;}
				if(temp.nodeData.nodeIndex>=sNodes.treeNodes[i].nodeData.nodeIndex){
					// Float ff = new Float(SubSolution.get(j));
					// sNodes.treeNodes[asf.adjacencies[j]].nodeData.value=(float)(SubSolution.get(j));
					// sNodes.treeNodes[asf.adjacencies[j]].nodeData.value= ff.floatValue();
					sNodes.treeNodes[asf.adjacencies[j]].nodeData.value= (float)SubSolution.get(j);
					// System.err.println("SubSolution[" + j + "] =" + SubSolution.get(j));
				}
			}
			// pause();
			
			double t=System.currentTimeMillis();
			// Update the values in the next depth
			int x1,x2,y1,y2,z1,z2;
			if(depth<sNodes.maxDepth-1){
				int idx1,idx2;
				OctNode node1, node2;
				// First pass: idx2 is the solution coefficient propogated
				for(j=0;j<matrix.rows;j++){
					idx1=asf.adjacencies[j];
					node1=sNodes.treeNodes[idx1];
					if(node1.children == null ){continue;}
					x1= (int)(node1.off[0]);
					y1= (int)(node1.off[1]);
					z1= (int)(node1.off[2]);

					for(int k=0;k<matrix.rowSizes[j];k++){
						idx2=asf.adjacencies[matrix.m_ppElements[j][k].N];
						node2=sNodes.treeNodes[idx2];
						temp=node2;
						while(temp.depth()>d){temp=temp.parent;}
						if(temp!=sNodes.treeNodes[i]){continue;}
						lpf.value=(float)(SubSolution.get(matrix.m_ppElements[j][k].N));
						x2= (int)(node2.off[0]);
						y2= (int)(node2.off[1]);
						z2= (int)(node2.off[2]);
						lpf.index[0]=x2;
						lpf.index[1]=y2;
						lpf.index[2]=z2;
						dx=(float)(x2-x1)/(1<<depth);
						dy=(float)(y2-y1)/(1<<depth);
						dz=(float)(z2-z1)/(1<<depth);
						if((float)Math.abs(dx)<myRadius2 && (float)Math.abs(dy)<myRadius2 && (float)Math.abs(dz)<myRadius2){
							node1.processNodeNodes(node2,lpf,0);}
						else{OctNode.ProcessNodeAdjacentNodes(fData.depth,node2,width,node1,width,lpf,0);}
					}
				}
				// Second pass: idx1 is the solution coefficient propogated
				for(j=0;j<matrix.rows;j++){
					idx1=asf.adjacencies[j];
					node1=sNodes.treeNodes[idx1];
					temp=node1;
					while(temp.depth()>d){temp=temp.parent;}
					if(temp!=sNodes.treeNodes[i]){continue;}
					x1= (int)(node1.off[0]);
					y1= (int)(node1.off[1]);
					z1= (int)(node1.off[2]);

					lpf.value=(float)(SubSolution.get(j));
					lpf.index[0]=x1;
					lpf.index[1]=y1;
					lpf.index[2]=z1;
					for(int k=0;k<matrix.rowSizes[j];k++){
						idx2=asf.adjacencies[matrix.m_ppElements[j][k].N];
						node2=sNodes.treeNodes[idx2];
						if( node2.children == null ){continue;}

						if(idx1!=idx2){
							x2= (int)(node2.off[0]);
							y2= (int)(node2.off[1]);
							z2= (int)(node2.off[2]);
							dx=(float)(x1-x2)/(1<<depth);
							dy=(float)(y1-y2)/(1<<depth);
							dz=(float)(z1-z2)/(1<<depth);
							if((float)Math.abs(dx)<myRadius2 && (float)Math.abs(dy)<myRadius2 && (float)Math.abs(dz)<myRadius2){
								node2.processNodeNodes(node1,lpf,0);}
							else{OctNode.ProcessNodeAdjacentNodes(fData.depth,node1,width,node2,width,lpf,0);}
						}
					}
				}
			
			}
			uTime=(System.currentTimeMillis()-uTime) / 1000f;
			System.err.println("\t\tGot / Solved / Updated in: " + gTime + " / " + sTime + " / "  + uTime);
			asf.adjacencies = null;
		   
		}
		
		return iter;
		
	}

	public int HasNormals(OctNode node, final float epsilon) {
		int hasNormals = 0;
		if (node.nodeData.nodeIndex >= 0
				&& geo.Length(normals.get(node.nodeData.nodeIndex)) > epsilon) {
			hasNormals = 1;
		}
		if (node.children != null) {
			for (int i = 0; i < Cube.CORNERS && hasNormals == 0; i++) {
				hasNormals |= HasNormals(node.children[i], epsilon);
			}
		}

		return hasNormals;
	}

	public void printTree() {
		OctNode temp;
		int i;
		temp = tree.nextNode(null);
		while ( temp != null ) {
			if ( temp.children != null ) {
				System.err.println("next");
				for ( i = 0; i < Cube.CORNERS; i++ ) {
					final OctNode n = temp.children[i];
					if ( n.children != null ) {
						System.err.println("n.nodeData.value = " + n.nodeData.value + " n.nodeData.nodeIndex = " + n.nodeData.nodeIndex);
						
					}
				}
				pause();
			}
			
			temp = tree.nextNode(temp);
		}
	}
	
	public void ClipTree() {
		OctNode temp;
		temp = tree.nextNode(null);
		while (temp != null) {
			if (temp.children != null) {
				int hasNormals = 0;
				for (int i = 0; i < Cube.CORNERS && ( hasNormals == 0 ? true : false ) ; i++) {
					hasNormals = HasNormals(temp.children[i], EPSILON);
				}
				if (hasNormals == 0) {
					temp.children = null;
				}
			}
			temp = tree.nextNode(temp);
		}
	}

	public void SetLaplacianWeights(){
		OctNode temp;

		fData.setDotTables(fData.DOT_FLAG | fData.D_DOT_FLAG);
		DivergenceFunction df = new DivergenceFunction();
		df.ot=this;
		temp=tree.nextNode(null);
		while(temp != null ){
			if(temp.nodeData.nodeIndex<0 || geo.Length( normals.get(temp.nodeData.nodeIndex))<=EPSILON){
				temp=tree.nextNode(temp);
				continue;
			}
			int d=temp.depth();

			df.normal= normals.get(temp.nodeData.nodeIndex);
			df.index[0]= (int)(temp.off[0]);
			df.index[1]= (int)(temp.off[1]);
			df.index[2]= (int)(temp.off[2]);
			
			/*
		    System.err.println("df.normal.coords[0] = " + df.normal.coords[0] + " df.normal.coords[1] = " + df.normal.coords[1]+ " df.normal.coords[2] = "+ df.normal.coords[2]);	
		    System.err.println("df.index[0] = " + df.index[0] + " df.index[1] = " + df.index[1]+ " df.index[2] = "+ df.index[2]);
			System.err.println("temp.nodeData.nodeIndex = " + temp.nodeData.nodeIndex );
			pause();
		    */
			
			OctNode.ProcessNodeAdjacentNodes(fData.depth,temp,width,tree,width,df, 1);
			// printTree();
			// System.err.println("df.index[0] = " + df.index[0] + " df.index[1] = " + df.index[1]+ " df.index[2] = "+ df.index[2]);
			// pause();
			
			temp=tree.nextNode(temp);
		}
		// printTree();
		
		fData.clearDotTables(fData.D_DOT_FLAG);
		temp=tree.nextNode(null);
		while(temp != null ){
			if(temp.nodeData.nodeIndex<0){temp.nodeData.centerWeightContribution=0;}
			else{temp.nodeData.centerWeightContribution=(float)(geo.Length(normals.get(temp.nodeData.nodeIndex)));}
			// System.err.println("temp.nodeData.centerWeightContribution = " + temp.nodeData.centerWeightContribution);
			// pause();
			temp=tree.nextNode(temp);
		}
		
		// MemoryUsage();
		normals=null;
	}

	public void GetMCIsoTriangles(final float isoValue, CoredMeshData mesh,
			final int fullDepthIso, final int nonLinearFit) {
		double t;
		OctNode temp;

		HashMap<Long, Integer> roots = new HashMap<Long, Integer>();
		HashMap<Long, Pair<Float, Point3D>> normalHash = new HashMap<Long, Pair<Float, Point3D>>();

		SetIsoSurfaceCorners(isoValue, 0, fullDepthIso);
		// At the point all of the corner values have been set and all nodes are
		// valid. Now it's just a matter
		// of running marching cubes.

		t = System.currentTimeMillis();
		fData.setValueTables(FunctionData.VALUE_FLAG | FunctionData.D_VALUE_FLAG, 0,
				postNormalSmooth);
		temp = tree.nextLeaf(null);
		while (temp != null) {
			SetMCRootPositions(temp, 0, isoValue, roots, null, normalHash,
					null, null, mesh, nonLinearFit);
			temp = tree.nextLeaf(temp);
		}
		// MemoryUsage();

		System.err.println("Normal Size: " + (double)(normalHash.size())/1000000 + " MB\n");
		System.err.println("Set " + mesh.inCorePoints.size()
				+ " root positions in: " + (System.currentTimeMillis() - t) / 1000f);
		// DumpOutput("Memory Usage: %.3f MB\n",float(MemoryUsage()));

		fData.clearValueTables();
		normalHash = null;

		// DumpOutput("Post deletion size: %.3f MB\n",float(MemoryUsage()));

		t = System.currentTimeMillis();

		// Now get the iso-surfaces, running from finest nodes to coarsest in
		// order to allow for edge propogation from
		// finer faces to coarser ones.
		temp = tree.nextLeaf(null);
		while (temp != null) {
			GetMCIsoTriangles(temp, mesh, roots, null, null, 0, 0);
			temp = tree.nextLeaf(temp);
		}
		System.err.println("Added triangles in: " + (System.currentTimeMillis() - t) / 1000f);
		// DumpOutput("Memory Usage: %.3f MB\n",float(MemoryUsage()));
	}

	public void GetMCIsoTriangles(final Float isoValue, final int subdivideDepth,
			CoredMeshData mesh, final int fullDepthIso, final int nonLinearFit) {
		OctNode temp;
		HashMap<Long, Integer> boundaryRoots = new HashMap<Long, Integer>();
		HashMap<Long, Integer> interiorRoots;
		HashMap<Long, Pair<Float, Point3D>> boundaryNormalHash, interiorNormalHash;
		Vector<Point3D> interiorPoints;

		int sDepth;
		if (subdivideDepth <= 0) {
			sDepth = 0;
		} else {
			sDepth = fData.depth - subdivideDepth;
		}
		if (sDepth < 0) {
			sDepth = 0;
		}

		SetIsoSurfaceCorners(isoValue, sDepth, fullDepthIso);
		// At this point all of the corner values have been set and all nodes
		// are valid. Now it's just a matter
		// of running marching cubes.

		boundaryNormalHash = new HashMap<Long, Pair<Float, Point3D>>();
		int offSet = 0;
		SortedTreeNodes sNodes = new SortedTreeNodes();
		sNodes.set(tree, 0);
		
		/*
		for (int j = 0; j < sNodes.maxDepth; j++ ) {
			System.err.println("Depth: " + j);
			for(int i=sNodes.nodeCount[j];i<sNodes.nodeCount[j+1];i++){
				System.err.println("(float)sNodes.treeNodes[" + i + "].nodeData.value = " + (float)sNodes.treeNodes[i].nodeData.value);
				System.err.println("(float)sNodes.treeNodes[" + i + "].nodeData.nodeIndex = " + (float)sNodes.treeNodes[i].nodeData.nodeIndex);
				System.err.println("(float)sNodes.treeNodes[" + i + "].nodeData.mcIndex = " + (float)sNodes.treeNodes[i].nodeData.mcIndex);
				
			}
			pause();
		}
		*/
		
		fData.setValueTables(fData.VALUE_FLAG | fData.D_VALUE_FLAG, 0, postNormalSmooth);

		// Set the root positions for all leaf nodes below the subdivide
		// threshold
		SetBoundaryMCRootPositions(sDepth, isoValue, boundaryRoots, boundaryNormalHash, mesh, nonLinearFit);

		for (int i = sNodes.nodeCount[sDepth]; i < sNodes.nodeCount[sDepth + 1]; i++) {
			interiorRoots = new HashMap<Long, Integer>();
			interiorNormalHash = new HashMap<Long, Pair<Float, Point3D>>();
			interiorPoints = new Vector<Point3D>();

			temp = sNodes.treeNodes[i].nextLeaf(null);
			while (temp != null) {
				if (1 == MarchingCubes.HasRoots(temp.nodeData.mcIndex)) {
					SetMCRootPositions(temp, sDepth, isoValue, boundaryRoots,
							interiorRoots, boundaryNormalHash,
							interiorNormalHash, interiorPoints, mesh,
							nonLinearFit);
				}
				temp = sNodes.treeNodes[i].nextLeaf(temp);
			}
			interiorNormalHash = null;

			temp = sNodes.treeNodes[i].nextLeaf(null);
			while (temp != null) {
				GetMCIsoTriangles(temp, mesh, boundaryRoots, interiorRoots,
						interiorPoints, offSet, sDepth);
				temp = sNodes.treeNodes[i].nextLeaf(temp);
			}
			interiorRoots = null;
			interiorPoints = null;
			offSet = mesh.outOfCorePointCount();
		}
		boundaryNormalHash = null;

		temp = tree.nextLeaf(null);
		while (temp != null) {
			if (temp.depth() < sDepth) {
				GetMCIsoTriangles(temp, mesh, boundaryRoots, null, null, 0, 0);
			}
			temp = tree.nextLeaf(temp);
		}
	}

	public float getCenterValue(final OctNode node) {
		int[] idx = new int[3];
		float value = 0;

		neighborKey2.getNeighbors(node);
		VertexData.CenterIndex(node, fData.depth, idx);
		idx[0] *= fData.res;
		idx[1] *= fData.res;
		idx[2] *= fData.res;
		// System.err.println("idx[0] = " + idx[0] + " idx[1] = " + idx[1] + " idx[2] = " + idx[2]);
		// pause();
		for (int i = 0; i <= node.depth(); i++) {
			for (int j = 0; j < 3; j++) {
				for (int k = 0; k < 3; k++) {
					for (int l = 0; l < 3; l++) {
						final OctNode n = neighborKey2.neighbors[i].neighbors[j][k][l];
						if (n != null) {
							float temp = n.nodeData.value;
							// System.err.println("temp = " + neighborKey2.neighbors[i].neighbors[j][k][l].nodeData.value);
							value += temp
									* (float) (fData.valueTables[idx[0] + (int)(n.off[0])]
											* fData.valueTables[idx[1] + (int)(n.off[1])] 
											* fData.valueTables[idx[2] + (int)(n.off[2])]);
							// System.err.println("n.off[0] = "+ n.off[0]+ " n.off[1] = " + n.off[1] + " n.off[2] = "+ n.off[2]);
							// System.err.println("f1 = " + fData.valueTables[idx[0] + (int) (n.off[0])] + " f2 = " +
							//  		fData.valueTables[idx[1] + (int) (n.off[1])] + " f3 = " + fData.valueTables[idx[2] + (int) (n.off[2])]);
							// System.err.println("temp = " + temp);
							// pause();
						}
					}
				}
			}
		}
	
		if (node.children != null) {
			for (int i = 0; i < Cube.CORNERS; i++) {
				int ii = Cube.AntipodalCornerIndex(i);
				OctNode n = node.children[i];
				while (true) {
					value += n.nodeData.value
							* (float) (fData.valueTables[idx[0] + (int)(n.off[0])]
							         * fData.valueTables[idx[1] + (int)(n.off[1])] 
							         * fData.valueTables[idx[2] + (int)(n.off[2])]);
					// System.err.println("value = " + value);
					// pause();
					if (n.children != null) {
						n = n.children[ii];
					} else {
						break;
					}
				}
			}
		}
		
		// System.err.println("value = " + value);
		// pause();
		return value;
	}

	public float getCornerValue(final OctNode node, final int corner) {
		int[] idx = new int[3];
		float value = 0f;

		neighborKey2.getNeighbors(node);
		VertexData.CornerIndex(node, corner, fData.depth, idx);
		idx[0] *= fData.res;
		idx[1] *= fData.res;
		idx[2] *= fData.res;
		for (int i = 0; i <= node.depth(); i++) {
			for (int j = 0; j < 3; j++) {
				for (int k = 0; k < 3; k++) {
					for (int l = 0; l < 3; l++) {
						final OctNode n = neighborKey2.neighbors[i].neighbors[j][k][l];
						if (n != null) {
							float temp = n.nodeData.value;
							value += temp
									* (float) (fData.valueTables[idx[0] + (int)(n.off[0])]
											* fData.valueTables[idx[1] + (int)(n.off[1])] 
											* fData.valueTables[idx[2] + (int)(n.off[2])]);
						}
					}
				}
			}
		}
		int d = node.depth();
		int[] x = new int[1];
		int[] y = new int[1];
		int[] z = new int[1];
		Cube.FactorCornerIndex(corner, x, y, z);
		for (int i = 0; i < 2; i++) {
			for (int j = 0; j < 2; j++) {
				for (int k = 0; k < 2; k++) {
					OctNode n = neighborKey2.neighbors[d].neighbors[x[0] + i][y[0]+ j][z[0] + k];
					if (n != null ) {
						int ii = Cube.AntipodalCornerIndex(Cube.CornerIndex(i, j, k));
						while (n.children != null) {
							n = n.children[ii];
							value += n.nodeData.value
									* (float) (fData.valueTables[idx[0] + (int)(n.off[0])]
											* fData.valueTables[idx[1] + (int)(n.off[1])] 
											* fData.valueTables[idx[2] + (int)(n.off[2])]);
						}
					}
				}
			}
		}
		return value;
	}

	public void getCornerValueAndNormal(final OctNode node, final int corner,
			float[] value, Point3D normal) {
		int[] idx = new int[3];
		int[] index = new int[3];
		value[0] = normal.coords[0] = normal.coords[1] = normal.coords[2] = 0;

		neighborKey2.getNeighbors(node);
		VertexData.CornerIndex(node, corner, fData.depth, idx);
		idx[0] *= fData.res;
		idx[1] *= fData.res;
		idx[2] *= fData.res;
		for (int i = 0; i <= node.depth(); i++) {
			for (int j = 0; j < 3; j++) {
				for (int k = 0; k < 3; k++) {
					for (int l = 0; l < 3; l++) {
						final OctNode n = neighborKey2.neighbors[i].neighbors[j][k][l];
						if (n != null) {
							float temp = n.nodeData.value;
							index[0] = idx[0] + (int)(n.off[0]);
							index[1] = idx[1] + (int)(n.off[1]);
							index[2] = idx[2] + (int)(n.off[2]);
							value[0] += temp
									* (float) (fData.valueTables[index[0]]
											* fData.valueTables[index[1]] 
											* fData.valueTables[index[2]]);
							normal.coords[0] += temp
									* (float)(fData.dValueTables[index[0]]
											* fData.valueTables[index[1]]
											* fData.valueTables[index[2]]);
							normal.coords[1] += temp
									* (float)(fData.valueTables[index[0]]
											* fData.dValueTables[index[1]]
											* fData.valueTables[index[2]]);
							normal.coords[2] += temp
									* (float)(fData.valueTables[index[0]]
											* fData.valueTables[index[1]]
											* fData.dValueTables[index[2]]);
						}
					}
				}
			}
		}
		int d = node.depth();
		int[] x = new int[1];
		int[] y = new int[1];
		int[] z = new int[1];
		Cube.FactorCornerIndex(corner, x, y, z);
		for (int i = 0; i < 2; i++) {
			for (int j = 0; j < 2; j++) {
				for (int k = 0; k < 2; k++) {
					OctNode n = neighborKey2.neighbors[d].neighbors[x[0] + i][y[0] + j][z[0] + k];
					if (n != null) {
						int ii = Cube.AntipodalCornerIndex(Cube.CornerIndex(i, j, k));
						while (n.children != null) {
							n = n.children[ii];
							float temp = n.nodeData.value;
							index[0] = idx[0] + (int)(n.off[0]);
							index[1] = idx[1] + (int)(n.off[1]);
							index[2] = idx[2] + (int)(n.off[2]);
							value[0] += temp
									* (float)(fData.valueTables[index[0]]
											* fData.valueTables[index[1]]
											* fData.valueTables[index[2]]);
							normal.coords[0] += temp
									* (float)(fData.dValueTables[index[0]]
											* fData.valueTables[index[1]]
											* fData.valueTables[index[2]]);
							normal.coords[1] += temp
									* (float)(fData.valueTables[index[0]]
											* fData.dValueTables[index[1]]
											* fData.valueTables[index[2]]);
							normal.coords[2] += temp
									* (float)(fData.valueTables[index[0]]
											* fData.valueTables[index[1]]
											* fData.dValueTables[index[2]]);
						}
					}
				}
			}
		}
	}

	public float GetIsoValue() {
		if (this.width <= 3) {
			OctNode temp;
			float isoValue, weightSum, w;

			neighborKey2.set(fData.depth);
			
			fData.setValueTables(fData.VALUE_FLAG, 0);

			isoValue = weightSum = 0;
			temp = tree.nextNode(null);
			while (temp != null) {
				w = temp.nodeData.centerWeightContribution;
				if (w > EPSILON) {
					isoValue += getCenterValue(temp) * w;
					weightSum += w;
					// System.err.println("isoValue = " + isoValue + " weightSum = " + weightSum + " w = " + w);
					// pause();
				}
				temp = tree.nextNode(temp);
			}
			return isoValue / weightSum;
		} else {
			OctNode temp;
			float isoValue, weightSum, w;
			float myRadius;
			PointIndexValueFunction cf = new PointIndexValueFunction();

			fData.setValueTables(fData.VALUE_FLAG, 0);
			cf.valueTables = fData.valueTables;
			cf.res2 = fData.res2;
			myRadius = radius;
			isoValue = weightSum = 0;
			temp = tree.nextNode(null);
			while (temp != null) {
				w = temp.nodeData.centerWeightContribution;
				if (w > EPSILON) {
					cf.value = 0;
					int[] idx = new int[3];
					VertexData.CenterIndex(temp, fData.depth, idx);
					cf.index[0] = idx[0] * fData.res;
					cf.index[1] = idx[1] * fData.res;
					cf.index[2] = idx[2] * fData.res;
					OctNode.ProcessPointAdjacentNodes(fData.depth, idx, tree, width, cf, 1);
					isoValue += cf.value * w;
					weightSum += w;
				}
				temp = tree.nextNode(temp);
			}
			return isoValue / weightSum;
		}
	}

	public void SetIsoSurfaceCorners(final float isoValue, final int subdivideDepth, final int fullDepthIso) {
		double t = System.currentTimeMillis();
		int i, j;
		HashMap<Long, Float> values = new HashMap<Long, Float>();
		float[] cornerValues = new float[Cube.CORNERS];
		PointIndexValueFunction cf = new PointIndexValueFunction();
		OctNode temp;
		int pIndex;
		int id;
		int leafCount = tree.leaves();
		long key;
		SortedTreeNodes sNodes = new SortedTreeNodes();
		sNodes.set(tree, 0);
		temp = tree.nextNode(null);
		while (temp != null) {
			temp.nodeData.mcIndex = 0;
			temp = tree.nextNode(temp);
		}
		TreeNodeData.UseIndex = 0;
		// Start by setting the corner values of all the nodes
		// cf.valueTables = fData.valueTables;
		cf.valueTables = new float[fData.valueTables.length];
		System.arraycopy(fData.valueTables, 0, cf.valueTables, 0, fData.valueTables.length);
		cf.res2 = fData.res2;
		
		for (i = 0; i < sNodes.nodeCount[subdivideDepth]; i++) {
			temp = sNodes.treeNodes[i];
			if (temp.children == null) {
				for (j = 0; j < Cube.CORNERS; j++) {
					if (this.width <= 3) {
						cornerValues[j] = getCornerValue(temp, j);
					} else {
						cf.value = 0;
						int[] idx = new int[3];
						VertexData.CornerIndex(temp, j, fData.depth, idx);
						cf.index[0] = idx[0] * fData.res;
						cf.index[1] = idx[1] * fData.res;
						cf.index[2] = idx[2] * fData.res;
						OctNode.ProcessPointAdjacentNodes(fData.depth, idx, tree, width, cf, 1);
						cornerValues[j] = cf.value;
					}
				}
				temp.nodeData.mcIndex = MarchingCubes.GetIndex(cornerValues, isoValue);
                /* 
				System.err.println("temp.nodeData.mcIndex  = " + temp.nodeData.mcIndex);
				for ( int k = 0; k < cornerValues.length; k++ ) {
					System.err.println("cornerValues[" + k + " ] = " + cornerValues[k]);
				}
				*/
				if (temp.parent != null) {
					OctNode parent = temp.parent;
					pIndex = 0;
					// int c = (int) (temp - temp.parent.children);
					for (id = 0; id < temp.parent.children.length; id++ ) {
						if ( temp == temp.parent.children[id] ) {
							pIndex = id;
							break;
						}
					}
					int c = pIndex;
					int mcid = temp.nodeData.mcIndex & (1 << MarchingCubes.cornerMap[c]);

					if (mcid != 0) {
						parent.nodeData.mcIndex |= mcid;
						while (true) {
							pIndex = 0;
							if ( parent.parent != null ) {
								for (id = 0; id < parent.parent.children.length; id++ ) {
									if ( parent == parent.parent.children[id] ) {
										pIndex = id;
										break;
									}
								}
							}
							// if (parent.parent != null
							//		&& (parent - parent.parent.children) == c) {
							if (parent.parent != null && pIndex == c) {
								parent.parent.nodeData.mcIndex |= mcid;
								parent = parent.parent;
							} else {
								break;
							}
						}
					}
				}
			}
		}
     
	     /*
		for (int jj = 0; jj < sNodes.maxDepth; jj++ ) {
			System.err.println("Depth: " + jj);
			for(int ii=sNodes.nodeCount[jj];ii<sNodes.nodeCount[jj+1];ii++){
				System.err.println("(float)sNodes.treeNodes[" + ii + "].nodeData.value = " + (float)sNodes.treeNodes[ii].nodeData.value + 
						"   (float)sNodes.treeNodes[" + ii + "].nodeData.nodeIndex = " + (float)sNodes.treeNodes[ii].nodeData.nodeIndex);
				
			}
			pause();
		}
	    */
		
		// MemoryUsage();

		
		
		for (i = sNodes.nodeCount[subdivideDepth]; i < sNodes.nodeCount[subdivideDepth + 1]; i++) {
			temp = sNodes.treeNodes[i].nextLeaf(null);
			while (temp != null) {
				for (j = 0; j < Cube.CORNERS; j++) {
					int[] idx = new int[3];
					key = VertexData.CornerIndex(temp, j, fData.depth, idx);
					cf.index[0] = idx[0] * fData.res;
					cf.index[1] = idx[1] * fData.res;
					cf.index[2] = idx[2] * fData.res;
					/*
					if ( values.size() > 0 ) {
					   keyArray =  values.keySet().toArray();
					   lastKey = (Long)keyArray[keyArray.length - 1];
					}
					*/
					if ( values.get(key) != null /* && values.get(key) != values.get(lastKey) */ ) {
						cornerValues[j] = values.get(key);
						//System.err.println("!=:   cornerValues[" + j + "] = " + cornerValues[j]);
					} else {
						if (this.width <= 3) {
							// values[key] = cornerValues[j] = getCornerValue( temp, j);
							cornerValues[j] = getCornerValue( temp, j);
							values.put(key, cornerValues[j]);
							///System.err.println("<3:   cornerValues[" + j + "] = " + cornerValues[j]);
							// System.err.println("<3:  key = " + key + " getCornerValue = " + getCornerValue(temp, j));
							
						} else {
							cf.value = 0;
							OctNode.ProcessPointAdjacentNodes(fData.depth, idx, tree, width, cf, 1);
							// values[key] = cf.value;
							values.put(key, cf.value);
							// System.err.println(">3:  key = " + key + " cf =  " + cf.value);
							cornerValues[j] = cf.value;
							///System.err.println(">3:   cornerValues[" + j + "] = " + cornerValues[j]);
						}
					}
				}
				
			
				temp.nodeData.mcIndex = MarchingCubes.GetIndex(cornerValues, isoValue);

				if (temp.parent != null) {
					OctNode parent = temp.parent;
					// int c = (int) (temp - temp.parent.children);
					pIndex = 0;
					for (id = 0; id < temp.parent.children.length; id++ ) {
						if ( temp == temp.parent.children[id] ) {
							pIndex = id;
							break;
						}
					}
					int c = pIndex;
					int mcid = temp.nodeData.mcIndex & (1 << MarchingCubes.cornerMap[c]);

					if (mcid != 0) {
						parent.nodeData.mcIndex |= mcid;
						while (true) {
							pIndex = 0;
							if ( parent.parent != null ) {
								for (id = 0; id < parent.parent.children.length; id++ ) {
									if ( parent == parent.parent.children[id] ) {
										pIndex = id;
										break;
									}
								}
							}
							// if (parent.parent != null
							//		&& (parent - parent.parent.children) == c) {
							if (parent.parent != null && pIndex == c) {
								parent.parent.nodeData.mcIndex |= mcid;
								parent = parent.parent;
							} else {
								break;
							}
						}
					}
				}
               
				temp = sNodes.treeNodes[i].nextLeaf(temp);
			}
			// MemoryUsage();
			values.clear();
		}
		sNodes = null;
		System.err.println("Set corner values in: " + ( System.currentTimeMillis()-t)/ 1000f);
		// DumpOutput("Memory Usage: %.3f MB\n",(float)(MemoryUsage()));
		/*
		sNodes = new SortedTreeNodes();
		sNodes.set(tree, 0);
		
		for (int jj = 0; jj < sNodes.maxDepth; jj++ ) {
			System.err.println("Depth: " + jj);
			for(int ii=sNodes.nodeCount[jj];ii<sNodes.nodeCount[jj+1];ii++){
				System.err.println("(float)sNodes.treeNodes[" + ii + "].nodeData.value = " + (float)sNodes.treeNodes[ii].nodeData.value + 
						"   (float)sNodes.treeNodes[" + ii + "].nodeData.nodeIndex = " + (float)sNodes.treeNodes[ii].nodeData.nodeIndex);
				
			}
			pause();
		}
	    */
		
		if (subdivideDepth != 0) {
			PreValidate(isoValue, fData.depth, subdivideDepth);
		}
	
	}

	public void Subdivide(OctNode node, final float isoValue, final int maxDepth) {
		int i, j;
		int[] c = new int[4];
		float value;
		int[] cornerIndex2 = new int[Cube.CORNERS];
		PointIndexValueFunction cf = new PointIndexValueFunction();
		cf.valueTables = fData.valueTables;
		cf.res2 = fData.res2;
		node.initChildren();
		// Since we are allocating blocks, it is possible that some of the
		// memory was pre-allocated with
		// the wrong initialization

		// Now set the corner values for the new children
		// Copy old corner values
		for (i = 0; i < Cube.CORNERS; i++) {
			cornerIndex2[i] = node.nodeData.mcIndex & (1 << MarchingCubes.cornerMap[i]);
		}
		// 8 of 27 corners set

		// Set center corner
		cf.value = 0;
		int[] idx = new int[3];
		VertexData.CenterIndex(node, maxDepth, idx);
		cf.index[0] = idx[0] * fData.res;
		cf.index[1] = idx[1] * fData.res;
		cf.index[2] = idx[2] * fData.res;
		if (this.width <= 3) {
			value = getCenterValue(node);
		} else {
			OctNode.ProcessPointAdjacentNodes(fData.depth, idx, tree, width, cf, 1);
			value = cf.value;
		}
		if (value < isoValue) {
			for (i = 0; i < Cube.CORNERS; i++) {
				cornerIndex2[i] |= 1 << MarchingCubes.cornerMap[Cube.AntipodalCornerIndex(i)];
			}
		}
		// 9 of 27 set

		// Set face corners
		for (i = 0; i < Cube.NEIGHBORS; i++) {
			int[] dir = new int[1];
			int[] offset = new int[1];
		
			int[] c0 = new int[1];
			int[] c1 = new int[1];
			int[] c2 = new int[1];
			int[] c3 = new int[1];
		
			int e;
			Cube.FactorFaceIndex(i, dir, offset);
			cf.value = 0;
			idx = new int[3];
			VertexData.FaceIndex(node, i, maxDepth, idx);
			cf.index[0] = idx[0] * fData.res;
			cf.index[1] = idx[1] * fData.res;
			cf.index[2] = idx[2] * fData.res;
			OctNode.ProcessPointAdjacentNodes(fData.depth, idx, tree, width, cf, 1);
			value = cf.value;
			
			Cube.FaceCorners(i, c0, c1, c2, c3);
			c[0] = c0[0];
			c[1] = c1[0];
			c[2] = c2[0];
			c[3] = c3[0];
			
			e = Cube.EdgeIndex(dir[0], 0, 0);
			if (value < isoValue) {
				for (j = 0; j < 4; j++) {
					cornerIndex2[c[j]] |= 1 << MarchingCubes.cornerMap[Cube.EdgeReflectCornerIndex(c[j], e)];
				}
			}
		}
		// 15 of 27 set

		// Set edge corners
		for (i = 0; i < Cube.EDGES; i++) {
			int[] o = new int[1];
			int[] i1 = new int[1];
			int[] i2 = new int[1];
			int[] c0 = new int[1];
			int[] c1 = new int[1];
			int f;
			Cube.FactorEdgeIndex(i, o, i1, i2);
			cf.value = 0;
			idx = new int[3];
			VertexData.EdgeIndex(node, i, maxDepth, idx);
			cf.index[0] = idx[0] * fData.res;
			cf.index[1] = idx[1] * fData.res;
			cf.index[2] = idx[2] * fData.res;
			OctNode.ProcessPointAdjacentNodes(fData.depth, idx, tree, width, cf, 1);
			value = cf.value;
			Cube.EdgeCorners(i, c0, c1);
			c[0] = c0[0];
			c[1] = c1[0];
			f = Cube.FaceIndex(o[0], 0);
			if (value < isoValue) {
				for (j = 0; j < 2; j++) {
					cornerIndex2[c[j]] |= 1 << MarchingCubes.cornerMap[Cube.FaceReflectCornerIndex(c[j], f)];
				}
			}
		}
		// 27 of 27 set

		for (i = 0; i < Cube.CORNERS; i++) {
			node.children[i].nodeData.mcIndex = cornerIndex2[i];
		}
	}

	public static int InteriorFaceRootCount(final OctNode node, final int faceIndex,
			final int maxDepth) {
		int c1, c2;
		int e1 = 0, e2 = 0;
		int cnt = 0;
		int[] dir = new int[1];
		int[] offf = new int[1];
		int[] corners = new int[Cube.CORNERS / 2];
		int[] corner0 = new int[1];
		int[] corner1 = new int[1];
		int[] corner2 = new int[1];
		int[] corner3 = new int[1];
		if (node.children != null) {
			Cube.FaceCorners(faceIndex, corner0, corner1, corner2, corner3);

			corners[0] = corner0[0];
			corners[1] = corner1[0];
			corners[2] = corner2[0];
			corners[3] = corner3[0];

			Cube.FactorFaceIndex(faceIndex, dir, offf);
			c1 = corners[0];
			c2 = corners[3];
			switch (dir[0]) {
			case 0:
				e1 = Cube.EdgeIndex(1, offf[0], 1);
				e2 = Cube.EdgeIndex(2, offf[0], 1);
				break;
			case 1:
				e1 = Cube.EdgeIndex(0, offf[0], 1);
				e2 = Cube.EdgeIndex(2, 1, offf[0]);
				break;
			case 2:
				e1 = Cube.EdgeIndex(0, 1, offf[0]);
				e2 = Cube.EdgeIndex(1, 1, offf[0]);
				break;
			}
			
			cnt += EdgeRootCount(node.children[c1], e1, maxDepth) + EdgeRootCount(node.children[c1], e2, maxDepth);
			switch (dir[0]) {
			case 0:
				e1 = Cube.EdgeIndex(1, offf[0], 0);
				e2 = Cube.EdgeIndex(2, offf[0], 0);
				break;
			case 1:
				e1 = Cube.EdgeIndex(0, offf[0], 0);
				e2 = Cube.EdgeIndex(2, 0, offf[0]);
				break;
			case 2:
				e1 = Cube.EdgeIndex(0, 0, offf[0]);
				e2 = Cube.EdgeIndex(1, 0, offf[0]);
				break;
			}
			
			cnt += EdgeRootCount(node.children[c2], e1, maxDepth) + EdgeRootCount(node.children[c2], e2, maxDepth);
			for (int i = 0; i < Cube.CORNERS / 2; i++) {
				if (node.children[corners[i]].children != null) {
					cnt += InteriorFaceRootCount(node.children[corners[i]], faceIndex, maxDepth);
				}
			}
		}
		return cnt;
	}

	public static int EdgeRootCount(final OctNode node, final int edgeIndex, final int maxDepth) {
		int[] c1 = new int[1];
		int[] c2 = new int[1];
		int[] f1 = new int[1];
		int[] f2 = new int[1];

		OctNode temp;
		Cube.FacesAdjacentToEdge(edgeIndex, f1, f2);

		int eIndex;
		OctNode finest = node;
		eIndex = edgeIndex;
		if (node.depth() < maxDepth) {
			temp = node.faceNeighbor(f1[0]);
			if (temp != null && temp.children != null) {
				finest = temp;
				eIndex = Cube.FaceReflectEdgeIndex(edgeIndex, f1[0]);
			} else {
				temp = node.faceNeighbor(f2[0]);
				if (temp != null && temp.children != null) {
					finest = temp;
					eIndex = Cube.FaceReflectEdgeIndex(edgeIndex, f2[0]);
				} else {
					temp = node.edgeNeighbor(edgeIndex);
					if (temp != null && temp.children != null) {
						finest = temp;
						eIndex = Cube.EdgeReflectEdgeIndex(edgeIndex);
					}
				}
			}
		}

		Cube.EdgeCorners(eIndex, c1, c2);
		if (finest.children != null) {
			return EdgeRootCount(finest.children[c1[0]], eIndex, maxDepth)
					+ EdgeRootCount(finest.children[c2[0]], eIndex, maxDepth);
		} else {
			return MarchingCubes.HasEdgeRoots(finest.nodeData.mcIndex, eIndex);
		}
	}

	public static int IsBoundaryFace(final OctNode node, final int faceIndex,
			final int subdivideDepth) {
		int[] dir = new int[1];
		int[] offset = new int[1];
		int[] dd = new int[1];
		int[] o = new int[3];
		int idx;

		if (subdivideDepth < 0) {
			return 0;
		}
		if ((int)(node.d[0]) <= subdivideDepth) {
			return 1;
		}
		Cube.FactorFaceIndex(faceIndex, dir, offset);
		node.depthAndOffset(dd, o);

		idx = ((int) (o[dir[0]]) << 1) + (offset[0] << 1);
		// return !(idx % (2 << ((int) (node.d[0]) - subdivideDepth)));
		return (idx % (2 << ((int) (node.d[0]) - subdivideDepth))) == 0 ? 1 : 0;
	}

	public static boolean IsBoundaryEdge(final OctNode node, final int edgeIndex,
			final int subdivideDepth) {
		int[] dir = new int[1];
		int[] x = new int[1];
		int[] y = new int[1];
		Cube.FactorEdgeIndex(edgeIndex, dir, x, y);
		return IsBoundaryEdge(node, dir[0], x[0], y[0], subdivideDepth);
	}

	public static boolean IsBoundaryEdge(final OctNode node, final int dir, final int x, final int y,
			final int subdivideDepth) {
		int[] dd = new int[1];
		int[] o = new int[3];
		int idx1 = 0, idx2 = 0, mask = 0;

		if (subdivideDepth < 0) {
			return false;
		}
		if ((int)(node.d[0]) <= subdivideDepth) {
			return true;
		}
		node.depthAndOffset(dd, o);

		switch (dir) {
		case 0:
			idx1 = ((int) (o[1]) << 1) + (x << 1);
			idx2 = ((int) (o[2]) << 1) + (y << 1);
			break;
		case 1:
			idx1 = ((int) (o[0]) << 1) + (x << 1);
			idx2 = ((int) (o[2]) << 1) + (y << 1);
			break;
		case 2:
			idx1 = ((int) (o[0]) << 1) + (x << 1);
			idx2 = ((int) (o[1]) << 1) + (y << 1);
			break;
		}
		mask = 2 << ( (int)(node.d[0]) - subdivideDepth);
		
		// return !(idx1%(mask)) || !(idx2%(mask));
		
		return ( (idx1%(mask)) == 0 ? true : false ) || ((idx2%(mask) == 0 ? true: false ));
	}

	public void PreValidate(OctNode node, final float isoValue, final int maxDepth,
			final int subdivideDepth) {
		int sub = 0;
		if (node.children != null) {
			System.err.println("Bad Pre-Validate\n");
		}
	
		
		// if(int(node->d)<subdivideDepth){sub=1;}
		for (int i = 0; i < Cube.NEIGHBORS && sub == 0; i++) {
			OctNode neighbor = node.faceNeighbor(i, 0);
			if (neighbor != null && neighbor.children != null) {	
				
				if (1 == IsBoundaryFace(node, i, subdivideDepth)
						&& 0 != InteriorFaceRootCount(neighbor, Cube.FaceReflectFaceIndex(i, i), maxDepth) ) {
					sub = 1;
				}
				
			}			
		}
		
		if (sub == 1) {
			Subdivide(node, isoValue, maxDepth);
			for (int i = 0; i < Cube.NEIGHBORS; i++) {
				if (1 == IsBoundaryFace(node, i, subdivideDepth) && 0 != InteriorFaceRootCount(node, i, maxDepth)) {
					OctNode neighbor = node.faceNeighbor(i, 0);
					while (neighbor != null && neighbor.children == null) {
						PreValidate(neighbor, isoValue, maxDepth,subdivideDepth);
						neighbor = node.faceNeighbor(i, 0);
					}
				}
			}
		}
	    
	}

	public void PreValidate(final float isoValue, final int maxDepth, final int subdivideDepth) {
		OctNode temp;
		temp = tree.nextLeaf(null);
		while (temp != null) {
			PreValidate(temp, isoValue, maxDepth, subdivideDepth);
			temp = tree.nextLeaf(temp);
		}
	}

	public void Validate(OctNode node, final float isoValue, final int maxDepth, final int fullDepthIso){
		int i,sub=0;
		OctNode treeNode=node;
		OctNode neighbor;
		if(node.depth()>=maxDepth || node.children != null ){return;}

		// Check if full-depth extraction is enabled and we have an iso-node
		// that is not at maximum depth
		if(sub == 0 && fullDepthIso == 1 && 1== MarchingCubes.HasRoots(node.nodeData.mcIndex)){sub=1;}

		// Check if the node has faces that are ambiguous and are adjacent to
		// finer neighbors
		for(i=0;i<Cube.NEIGHBORS && sub == 0;i++){
			neighbor=treeNode.faceNeighbor(i, 0);
			if(neighbor != null && neighbor.children != null ){
				if(MarchingCubes.IsAmbiguous(node.nodeData.mcIndex,i)){sub=1;}}
		}

		// Check if the node has edges with more than one root
		for(i=0;i<Cube.EDGES && sub == 0;i++){
			if(EdgeRootCount(node,i,maxDepth)>1){sub=1;}}

		for(i=0;i<Cube.NEIGHBORS && sub == 0;i++){
			neighbor=node.faceNeighbor(i, 0);
			if(	neighbor != null && neighbor.children != null &&
				!MarchingCubes.HasFaceRoots(node.nodeData.mcIndex,i) &&
				1== InteriorFaceRootCount(neighbor,Cube.FaceReflectFaceIndex(i,i),maxDepth)){sub=1;}
		}
		if(sub == 1){
			Subdivide(node,isoValue,maxDepth);
			for(i=0;i<Cube.NEIGHBORS;i++){
				neighbor=treeNode.faceNeighbor(i, 0);
				if(neighbor != null  && neighbor.children == null ){Validate(neighbor,isoValue,maxDepth,fullDepthIso);}
			}
			for(i=0;i<Cube.EDGES;i++){
				neighbor=treeNode.edgeNeighbor(i);
				if(neighbor != null && neighbor.children == null ){Validate(neighbor,isoValue,maxDepth,fullDepthIso);}
			}
			for(i=0;i<Cube.CORNERS;i++){if(node.children[i].children == null){Validate(node.children[i],isoValue,maxDepth,fullDepthIso);}}
		}
	}

	public void Validate(OctNode node, final float isoValue, final int maxDepth,
			final int fullDepthIso, final int subdivideDepth) {
		int i, sub = 0;
		OctNode treeNode = node;
		OctNode neighbor;
		if (node.depth() >= maxDepth || node.children != null) {
			return;
		}

		// Check if full-depth extraction is enabled and we have an iso-node that is not at maximum depth
		if (sub == 0 && fullDepthIso == 1
				&& 1 == MarchingCubes.HasRoots(node.nodeData.mcIndex)) {
			sub = 1;
		}

		// Check if the node has faces that are ambiguous and are adjacent to finer neighbors
		for (i = 0; i < Cube.NEIGHBORS && sub == 0; i++) {
			neighbor = treeNode.faceNeighbor(i, 0);
			if (neighbor != null && neighbor.children != null) {
				if (MarchingCubes.IsAmbiguous(node.nodeData.mcIndex, i)
						|| 1== IsBoundaryFace(node, i, subdivideDepth)) {
					sub = 1;
				}
			}
		}

		// Check if the node has edges with more than one root
		for (i = 0; i < Cube.EDGES && sub == 0; i++) {
			if (EdgeRootCount(node, i, maxDepth) > 1) {
				sub = 1;
			}
		}

		for (i = 0; i < Cube.NEIGHBORS && sub == 0; i++) {
			neighbor = node.faceNeighbor(i, 0);
			if (neighbor != null
					&& neighbor.children != null
					&& !MarchingCubes.HasFaceRoots(node.nodeData.mcIndex, i)
					&& 1 == InteriorFaceRootCount(neighbor, Cube.FaceReflectFaceIndex(i, i), maxDepth)) {
				sub = 1;
			}
		}
		if (sub == 1) {
			Subdivide(node, isoValue, maxDepth);
			for (i = 0; i < Cube.NEIGHBORS; i++) {
				neighbor = treeNode.faceNeighbor(i, 0);
				if (neighbor != null && neighbor.children == null) {
					Validate(neighbor, isoValue, maxDepth, fullDepthIso,
							subdivideDepth);
				}
			}
			for (i = 0; i < Cube.EDGES; i++) {
				neighbor = treeNode.edgeNeighbor(i);
				if (neighbor != null && neighbor.children == null) {
					Validate(neighbor, isoValue, maxDepth, fullDepthIso,
							subdivideDepth);
				}
			}
			for (i = 0; i < Cube.CORNERS; i++) {
				if (node.children[i].children == null ) {
					Validate(node.children[i], isoValue, maxDepth,
							fullDepthIso, subdivideDepth);
				}
			}
		}
	}

	//////////////////////////////////////////////////////////////////////////////////////
	// The assumption made when calling this code is that the edge has at most one root //
	//////////////////////////////////////////////////////////////////////////////////////
	public int GetRoot(final RootInfo ri, final float isoValue,Point3D position, HashMap<Long,Pair<Float,Point3D > > normalHash, final int nonLinearFit){
		int[] c1 = new int[1];
		int[] c2 = new int[1];
		Cube.EdgeCorners(ri.edgeIndex,c1,c2);
		if(0 == MarchingCubes.HasEdgeRoots(ri.node.nodeData.mcIndex,ri.edgeIndex)){return 0;}

		long key;
		Point3D[] n = new Point3D[2];
		n[0] = new Point3D();
		n[1] = new Point3D();
		PointIndexValueAndNormalFunction cnf = new PointIndexValueAndNormalFunction();
		cnf.valueTables=fData.valueTables;
		cnf.dValueTables=fData.dValueTables;
		cnf.res2=fData.res2;

		int i,rCount=0;
		int[] o = new int[1];
		int[] i1 = new int[1];
		int[] i2 = new int[1];
		Polynomial P = new Polynomial(2);
		Vector<Double> roots = new Vector<Double>();
		double x0,x1;
		float center;
		float[] width = new float[1];
		float averageRoot=0;
		Cube.FactorEdgeIndex(ri.edgeIndex,o,i1,i2);
		int[] idx = new int[3];
		key=VertexData.CornerIndex(ri.node,c1[0],fData.depth,idx);
		cnf.index[0]=idx[0]*fData.res;
		cnf.index[1]=idx[1]*fData.res;
		cnf.index[2]=idx[2]*fData.res;

		

		
		if(normalHash.get(key)==null){
			cnf.value[0]=0;
			cnf.normal.coords[0]=0;
			cnf.normal.coords[1]=0; 
			cnf.normal.coords[2]=0;
			// Careful here as the normal isn't quite accurate... (i.e. postNormalSmooth is ignored)
			if(this.width<=3){getCornerValueAndNormal(ri.node,c1[0],cnf.value,cnf.normal);}
			else{OctNode.ProcessPointAdjacentNodes(fData.depth,idx,tree,this.width,cnf, 1);}
			normalHash.put(key, new Pair<Float,Point3D >(cnf.value[0],cnf.normal));
		}
		x0=normalHash.get(key).first;
		n[0].coords[0] =normalHash.get(key).second.coords[0];
		n[0].coords[1] =normalHash.get(key).second.coords[1];
		n[0].coords[2] =normalHash.get(key).second.coords[2];
		
		/*
		System.err.println("first");
		System.err.println("key = " + key);
		System.err.println("x0 = " + x0);
		System.err.println("n[0] = " + n[0].coords[0] + " " + n[0].coords[1] + " " + n[0].coords[2]);
		pause();
        */
		key=VertexData.CornerIndex(ri.node,c2[0],fData.depth,idx);
		cnf.index[0]=idx[0]*fData.res;
		cnf.index[1]=idx[1]*fData.res;
		cnf.index[2]=idx[2]*fData.res;
		
		
		if(normalHash.get(key)==null){
			cnf.value[0]=0;
			cnf.normal.coords[0] = 0;
			cnf.normal.coords[1] = 0;
			cnf.normal.coords[2] = 0;
			if(this.width<=3){getCornerValueAndNormal(ri.node,c2[0],cnf.value,cnf.normal);}
			else{OctNode.ProcessPointAdjacentNodes(fData.depth,idx,tree,this.width,cnf, 1);}
			normalHash.put(key, new Pair<Float,Point3D>(cnf.value[0],cnf.normal));
		}
		x1=normalHash.get(key).first;
		n[1].coords[0]=normalHash.get(key).second.coords[0];
		n[1].coords[1]=normalHash.get(key).second.coords[1];
		n[1].coords[2]=normalHash.get(key).second.coords[2];
		/*
		System.err.println("Second");
		System.err.println("key = " + key);
		System.err.println("x1 = " + x1);
		System.err.println("n[1] = " + n[1].coords[0] + " " + n[1].coords[1] + " " + n[1].coords[2]);
		System.err.println("first");
		System.err.println("key = " + key);
		System.err.println("x0 = " + x0);
		System.err.println("n[0] = " + n[0].coords[0] + " " + n[0].coords[1] + " " + n[0].coords[2]);
		pause();
        */
		Point3D c = new Point3D();
		ri.node.centerAndWidth(c,width);
		center=c.coords[o[0]];
		for(i=0;i<DIMENSION;i++){
			n[0].coords[i]*=width[0];
			n[1].coords[i]*=width[0];
		}

		switch(o[0]){
					case 0:
						position.coords[1]=c.coords[1]-width[0]/2+width[0]*i1[0];
						position.coords[2]=c.coords[2]-width[0]/2+width[0]*i2[0];
						break;
					case 1:
						position.coords[0]=c.coords[0]-width[0]/2+width[0]*i1[0];
						position.coords[2]=c.coords[2]-width[0]/2+width[0]*i2[0];
						break;
					case 2:
						position.coords[0]=c.coords[0]-width[0]/2+width[0]*i1[0];
						position.coords[1]=c.coords[1]-width[0]/2+width[0]*i2[0];
						break;
		}
		double dx0,dx1;
		dx0=n[0].coords[o[0]];
		dx1=n[1].coords[o[0]];

		// The scaling will turn the Hermite Spline into a quadratic
		double scl=(x1-x0)/((dx1+dx0)/2);
		dx0*=scl;
		dx1*=scl;

		// Hermite Spline
		P.coefficients[0]=x0;
		P.coefficients[1]=dx0;
		P.coefficients[2]=3*(x1-x0)-dx1-2*dx0;

		P.getSolutions(isoValue,roots,EPSILON);
		for(i=0;i<(int)(roots.size());i++){
			if(roots.get(i)>=0 && roots.get(i)<=1){
				averageRoot+=(double)(roots.get(i));
				rCount++;
			}
		}
		if(rCount != 0 && nonLinearFit != 0)	{averageRoot/=rCount;}
		else						{averageRoot=(float)((x0-isoValue)/(x0-x1));}

		position.coords[o[0]]=(float)(center-width[0]/2+width[0]*averageRoot);
		return 1;
	}

	public int GetRoot(final RootInfo ri, final float isoValue, final int maxDepth,Point3D position, HashMap<Long, Pair<Float,Point3D > > normals,
			Point3D normal, final int nonLinearFit){
		if(0 == MarchingCubes.HasRoots(ri.node.nodeData.mcIndex)){return 0;}
		return GetRoot(ri,isoValue,position,normals,nonLinearFit);
	}
	
	public static int GetRootIndex(final OctNode node, final int edgeIndex, final int maxDepth, final int sDepth, RootInfo ri){
		int[] c1 = new int[1];
		int[] c2 = new int[1];
		int[] f1 = new int[1];
		int[] f2 = new int[1];
		OctNode temp,finest;
		int finestIndex;

		Cube.FacesAdjacentToEdge(edgeIndex,f1,f2);

		finest=node;
		finestIndex=edgeIndex;
		if(node.depth()<maxDepth){
			if(1== IsBoundaryFace(node,f1[0],sDepth)){temp=null;}
			else{temp=node.faceNeighbor(f1[0]);}
			if(temp != null && temp.children != null ){
				finest=temp;
				finestIndex=Cube.FaceReflectEdgeIndex(edgeIndex,f1[0]);
			}
			else{
				if(1== IsBoundaryFace(node,f2[0],sDepth)){temp=null;}
				else{temp=node.faceNeighbor(f2[0]);}
				if(temp != null && temp.children != null ){
					finest=temp;
					finestIndex=Cube.FaceReflectEdgeIndex(edgeIndex,f2[0]);
				}
				else{
					if(IsBoundaryEdge(node,edgeIndex,sDepth)){temp=null;}
					else{temp=node.edgeNeighbor(edgeIndex);}
					if(temp != null && temp.children != null ){
						finest=temp;
						finestIndex=Cube.EdgeReflectEdgeIndex(edgeIndex);
					}
				}
			}
		}

		Cube.EdgeCorners(finestIndex,c1,c2);
		if(finest.children != null ){
			if		(1 == GetRootIndex(finest.children[c1[0]],finestIndex,maxDepth,sDepth,ri))	{return 1;}
			else if	(1 == GetRootIndex(finest.children[c2[0]],finestIndex,maxDepth,sDepth,ri))	{return 1;}
			else																							{return 0;}
		}
		else{
			if(0 == (MarchingCubes.edgeMask[finest.nodeData.mcIndex] & (1<<finestIndex))){return 0;}

			int[] o = new int[1];
			int[] i1 = new int[1];
			int[] i2 = new int[1];
			Cube.FactorEdgeIndex(finestIndex,o,i1,i2);
			int[] d= new int[1];
			int[] offf = new int[3];
			finest.depthAndOffset(d,offf);
			ri.node=finest;
			ri.edgeIndex=finestIndex;
			int[] eIndex = new int[2];
			int offset;
			offset=BinaryNode.Index(d[0],offf[o[0]]);
			switch(o[0]){
					case 0:
						eIndex[0]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[1],i1[0]);
						eIndex[1]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[2],i2[0]);
						break;
					case 1:
						eIndex[0]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[0],i1[0]);
						eIndex[1]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[2],i2[0]);
						break;
					case 2:
						eIndex[0]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[0],i1[0]);
						eIndex[1]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[1],i2[0]);
						break;
			}
			ri.key = (long)(o[0]) | (long)(eIndex[0])<<5 | (long)(eIndex[1])<<25 | (long)(offset)<<45;
			return 1;
		}
	}
	
	public static boolean GetRootIndex(final OctNode node, final int edgeIndex, final int maxDepth,RootInfo ri){
		int[] c1 = new int[1];
		int[] c2 = new int[1];
		int[] f1 = new int[1];
		int[] f2 = new int[1];
		OctNode temp,finest;
		int finestIndex;


		// The assumption is that the super-edge has a root along it. 
		if( (MarchingCubes.edgeMask[node.nodeData.mcIndex] & (1<<edgeIndex)) == 0 ? true : false  ){
			return false;
		}

		Cube.FacesAdjacentToEdge(edgeIndex,f1,f2);

		finest=node;
		finestIndex=edgeIndex;
		if(node.depth()<maxDepth){
			temp=node.faceNeighbor(f1[0]);
			if(temp != null && temp.children != null ){
				finest=temp;
				finestIndex=Cube.FaceReflectEdgeIndex(edgeIndex,f1[0]);
			}
			else{
				temp=node.faceNeighbor(f2[0]);
				if(temp != null && temp.children != null ){
					finest=temp;
					finestIndex=Cube.FaceReflectEdgeIndex(edgeIndex,f2[0]);
				}
				else{
					temp=node.edgeNeighbor(edgeIndex);
					if(temp != null && temp.children != null){
						finest=temp;
						finestIndex=Cube.EdgeReflectEdgeIndex(edgeIndex);
					}
				}
			}
		}

		Cube.EdgeCorners(finestIndex,c1,c2);
		if(finest.children != null ){
			if		(GetRootIndex(finest.children[c1[0]],finestIndex,maxDepth,ri))				{return true;}
			else if	(GetRootIndex(finest.children[c2[0]],finestIndex,maxDepth,ri))				{return true;}
			else																				{return false;}
		}
		else{
			int[] o = new int[1];
			int[] i1 = new int[1];
			int[] i2 = new int[1];
			Cube.FactorEdgeIndex(finestIndex,o,i1,i2);
			int[] d = new int[1];
			int[] offf= new int[3];
			finest.depthAndOffset(d,offf);
			ri.node=finest;
			ri.edgeIndex=finestIndex;
			int offset;
			int[] eIndex = new int[2];
			offset=BinaryNode.Index(d[0],offf[o[0]]);
			switch(o[0]){
					case 0:
						eIndex[0]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[1],i1[0]);
						eIndex[1]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[2],i2[0]);
						break;
					case 1:
						eIndex[0]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[0],i1[0]);
						eIndex[1]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[2],i2[0]);
						break;
					case 2:
						eIndex[0]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[0],i1[0]);
						eIndex[1]=BinaryNode.CornerIndex(maxDepth+1,d[0],offf[1],i2[0]);
						break;
			}
			ri.key= (long)(o[0]) | (long)(eIndex[0])<<5 | (long)(eIndex[1])<<25 | (long)(offset)<<45;
			return true;
		}
	}
	
	public boolean GetRootPair(final RootInfo ri, final int maxDepth,RootInfo pair){
		OctNode node=ri.node;
		int pIndex = 0;
		int[] c1 = new int[1];
		int[] c2 = new int[1];
		int c;
		Cube.EdgeCorners(ri.edgeIndex,c1,c2);
		while(node.parent != null ){
			// c=(int)(node-node.parent.children);
			for (int id = 0; id < node.parent.children.length; id++ ) {
				if ( node == node.parent.children[id] ) {
					pIndex = id;
					break;
				}
			}
			c=(int)pIndex;
			if(c!=c1[0] && c!=c2[0]){return false;}
			if(0 == MarchingCubes.HasEdgeRoots(node.parent.nodeData.mcIndex,ri.edgeIndex)){
				if(c==c1[0]){return GetRootIndex(node.parent.children[c2[0]],ri.edgeIndex,maxDepth,pair);}
				else{return GetRootIndex(node.parent.children[c1[0]],ri.edgeIndex,maxDepth,pair);}
			}
			node=node.parent;
		}
		return false;

	}
	
	public static int GetRootIndex(final Long key, HashMap<Long,Integer> boundaryRoots,HashMap<Long,Integer> interiorRoots,CoredPointIndex index){
		// HashMap<Long,Integer> iterator rootIter=boundaryRoots.find(key);
		Integer rootIter = boundaryRoots.get(key);
		
		if(rootIter!=null){
			index.inCore=1;
			index.index= rootIter.intValue();
			return 1;
		}
		else if(interiorRoots != null){
			rootIter=interiorRoots.get(key);
			
			if(rootIter!=null){
				index.inCore=0;
				index.index=rootIter.intValue();
				return 1;
			}
		}
		return 0;
	}
	

	public int SetMCRootPositions(OctNode node, final int sDepth, final float isoValue,
										   HashMap<Long,Integer> boundaryRoots, HashMap<Long, Integer> interiorRoots,
										   HashMap<Long,Pair<Float,Point3D> > boundaryNormalHash, HashMap<Long, Pair<Float,Point3D > > interiorNormalHash,
										   Vector<Point3D > interiorPositions,
										   CoredMeshData mesh, final int nonLinearFit){
		
		int i,j,k,eIndex;
		RootInfo ri = new RootInfo();
		Point3D position = new Point3D();
		
		int count=0;
		if(0 == MarchingCubes.HasRoots(node.nodeData.mcIndex)){return 0;}
		for(i=0;i<DIMENSION;i++){
			for(j=0;j<2;j++){
				for(k=0;k<2;k++){
					long key;
					eIndex=Cube.EdgeIndex(i,j,k);
					if(GetRootIndex(node,eIndex,fData.depth,ri)){
						key=ri.key;
						if(interiorRoots == null || IsBoundaryEdge(node,i,j,k,sDepth)){
							
							if(boundaryRoots.get(key)==null){
								
								GetRoot(ri,isoValue,fData.depth,position,boundaryNormalHash,null,nonLinearFit);
								mesh.inCorePoints.add(new Point3D(position.coords[0], position.coords[1], position.coords[2]));
								boundaryRoots.put(key, (int)(mesh.inCorePoints.size())-1);
								/*
								System.err.println("first");
								System.err.println("position = " + position.coords[0] + " " + position.coords[1]+ " " + position.coords[2]);
								System.err.println("key = " + key + "  (int)(mesh.inCorePoints.size())-1 = " + ((int)(mesh.inCorePoints.size())-1));
								pause();
								*/
								count++;
							}
						}
						else{
							
							if(interiorRoots.get(key)==null){
								GetRoot(ri,isoValue,fData.depth,position,interiorNormalHash,null,nonLinearFit);
								interiorRoots.put(key, mesh.addOutOfCorePoint(new Point3D(position.coords[0], position.coords[1], position.coords[2])));
								interiorPositions.add(new Point3D(position.coords[0], position.coords[1], position.coords[2]));
								/*
								System.err.println("second");
								System.err.println("position = " + position.coords[0] + " " + position.coords[1]+ " " + position.coords[2]);
								System.err.println("key = " + key + "  mesh.addOutOfCorePoint(position) = " + mesh.addOutOfCorePoint(position));
								pause();
								*/
								count++;
							}
						}
					}
				}
			}
		}
		return count;
	}
	
	
	public int SetBoundaryMCRootPositions(final int sDepth, final float isoValue,
												   HashMap<Long, Integer> boundaryRoots,
												   HashMap<Long, Pair<Float,Point3D> > boundaryNormalHash,
												   CoredMeshData mesh, final int nonLinearFit){
		
		int i,j,k,eIndex,hits = 0;
		RootInfo ri = new RootInfo();
		int count=0;
		OctNode node;

		node=tree.nextLeaf(null);
		while(node != null){
			if(1== MarchingCubes.HasRoots(node.nodeData.mcIndex)){
				hits=0;
				for(i=0;i<DIMENSION;i++){
					for(j=0;j<2;j++){
						for(k=0;k<2;k++){
							if(IsBoundaryEdge(node,i,j,k,sDepth)){
								hits++;
								long key;
								eIndex=Cube.EdgeIndex(i,j,k);
								if(GetRootIndex(node,eIndex,fData.depth,ri)){
									key=ri.key;
									
									if(boundaryRoots.get(key)==null){
										Point3D position = new Point3D();
										GetRoot(ri,isoValue,fData.depth,position,boundaryNormalHash,null,nonLinearFit);
										mesh.inCorePoints.add(position);
										/*
										System.err.println("position[0] = " + position.coords[0] + " position[1] = " + position.coords[1] + " position[2] = " + position.coords[2]);
										System.err.println("key = " + key + " (int)(mesh.inCorePoints.size())-1 = " + ((int)(mesh.inCorePoints.size())-1));
										pause();
										*/
										boundaryRoots.put(key, (int)(mesh.inCorePoints.size())-1);
										count++;
									}
								}
							}
						}
					}
				}
			}
			if(hits != 0){node=tree.nextLeaf(node);}
			else{node=tree.nextBranch(node);}
		}
		return count;
	}	
	

	public void GetMCIsoEdges(OctNode node,
							HashMap<Long,Integer> boundaryRoots,
							HashMap<Long,Integer> interiorRoots,
							final int sDepth,
						     Vector<Pair<Long, Long>> edges){
		OctNode temp;
		int count=0,tris=0;
		int[] isoTri = new int[DIMENSION*MarchingCubes.MAX_TRIANGLES];
		FaceEdgesFunction fef = new FaceEdgesFunction();
		int ref,fIndex;
		// HashMap<Long,Pair<RootInfo,Integer> > iter;
		Pair<RootInfo,Integer> iter;

		HashMap<Long,Pair<RootInfo,Integer>> vertexCount = new HashMap<Long,Pair<RootInfo,Integer>>();

		fef.edges= edges;
		fef.maxDepth=fData.depth;
		fef.vertexCount= vertexCount;
		count=MarchingCubes.AddTriangleIndices(node.nodeData.mcIndex,isoTri);
		for(fIndex=0;fIndex<Cube.NEIGHBORS;fIndex++){
			ref=Cube.FaceReflectFaceIndex(fIndex,fIndex);
			fef.fIndex=ref;
			temp=node.faceNeighbor(fIndex, 0);
			// If the face neighbor exists and has higher resolution than the current node,
			// get the iso-curve from the neighbor
			if(temp != null && temp.children != null && ( IsBoundaryFace(node,fIndex,sDepth) == 0 ? true : false) ){
				temp.processNodeFaces(temp,fef,ref,1);}
			// Otherwise, get it from the node
			else{
				RootInfo ri1 = new RootInfo();
				RootInfo ri2 = new RootInfo();
				for(int j=0;j<count;j++){
					for(int k=0;k<3;k++){
						if(fIndex==Cube.FaceAdjacentToEdges(isoTri[j*3+k],isoTri[j*3+((k+1)%3)])){
							if(GetRootIndex(node,isoTri[j*3+k],fData.depth,ri1) && GetRootIndex(node,isoTri[j*3+((k+1)%3)],fData.depth,ri2)){
								edges.add(new Pair<Long, Long>(ri1.key,ri2.key));						
	
								iter=vertexCount.get(ri1.key);
								
								
								if(iter==null){
									vertexCount.put(ri1.key, new Pair(ri1, 0));
									// System.err.println("ri1.key = " + ri1.key);
									// vertexCount.get(ri1.key).first=ri1;
									// vertexCount.get(ri1.key).second=0;
								}
								iter=vertexCount.get(ri2.key);
								if(iter==null){
									vertexCount.put(ri2.key, new Pair(ri2, 0));
									// System.err.println("ri2.key = " + ri2.key);
									// vertexCount.get(ri2.key).first=ri2;
									// vertexCount.get(ri2.key).second=0;
								}
								// pause();
								vertexCount.get(ri1.key).second++;
								vertexCount.get(ri2.key).second--;
							}
							else{System.err.println("Bad Edge 1: " + ri1.key + " " +  ri2.key);}
						}
					}
				}
			}
		}
		for(int i=0;i<(int)(edges.size());i++){
			
			
			iter=vertexCount.get(edges.get(i).first);
			if(iter==null){
				System.err.println("Could not find vertex: " + edges.get(i).first);}
			else if(vertexCount.get(edges.get(i).first).second != 0){
				RootInfo ri = new RootInfo();
				GetRootPair(vertexCount.get(edges.get(i).first).first,fData.depth,ri);
				iter=vertexCount.get(ri.key);
				if(iter==null){System.err.println("Vertex pair not in list\n");}
				else{
					edges.add(new Pair<Long, Long>(ri.key,edges.get(i).first));
					vertexCount.get(ri.key).second++;
					vertexCount.get(edges.get(i).first).second--;
				}
			}

			
			iter=vertexCount.get(edges.get(i).second);
			if(iter==null){
				System.err.println("Could not find vertex: " + edges.get(i).second);}
			else if(vertexCount.get(edges.get(i).second).second != 0){
				RootInfo ri = new RootInfo();
				GetRootPair(vertexCount.get(edges.get(i).second).first,fData.depth,ri);
				iter=vertexCount.get(ri.key);
				if(iter==null){System.err.println("Vertex pair not in list\n");}
				else{
					edges.add(new Pair<Long,Long>(edges.get(i).second,ri.key));
					vertexCount.get(edges.get(i).second).second++;
					vertexCount.get(ri.key).second--;
				}
			}
		}
	}
	

	public int GetMCIsoTriangles(OctNode node,CoredMeshData mesh,HashMap<Long, Integer> boundaryRoots,
										  HashMap<Long,Integer> interiorRoots,
										  Vector<Point3D > interiorPositions,
										  final int offSet, final int sDepth)
	{
		int tris=0;
		Vector<Pair<Long,Long> > edges = new Vector<Pair<Long,Long>> ();
		Vector<Vector<Pair<Long,Long> >> edgeLoops = new Vector<Vector<Pair<Long,Long>>>();
		GetMCIsoEdges(node,boundaryRoots,interiorRoots,sDepth,edges);

		GetEdgeLoops(edges,edgeLoops);
		for(int i=0;i<(int)(edgeLoops.size());i++){
			CoredPointIndex p = new CoredPointIndex();
			Vector<CoredPointIndex> edgeIndices = new Vector<CoredPointIndex>();
			for(int j=0;j<(int)(edgeLoops.get(i).size());j++){
				if(0==GetRootIndex(edgeLoops.get(i).get(j).first,boundaryRoots,interiorRoots,p)){
					System.err.println("Bad Point Index\n");}
				else{edgeIndices.add(new CoredPointIndex(p.index, p.inCore));}
			}
			tris+=AddTriangles(mesh,edgeIndices,interiorPositions,offSet);
		}
		return tris;
	}

	
	public static int GetEdgeLoops(Vector<Pair<Long,Long> > edges,Vector<Vector<Pair<Long,Long> > > loops){
		int loopSize=0;
		long frontIdx,backIdx;
		Pair<Long,Long> e = (Pair<Long,Long>)new Pair(0, 0);
		Pair<Long,Long> temp = (Pair<Long,Long>)new Pair(0, 0) ;
		loops.clear();

		while(edges.size() != 0){
			Vector<Pair<Long,Long>> front = new Vector<Pair<Long,Long>>(); 
			Vector<Pair<Long,Long>> back = new Vector<Pair<Long,Long>>();
			e=edges.get(0);
			// loops.Resize(loopSize+1);
			loops.setSize(loopSize+1);
			edges.set(0, edges.get(edges.size()-1));
			// edges.pop_back();
			edges.removeElementAt(edges.size()-1);
			frontIdx=e.second;
			backIdx=e.first;
			for(int j=(int)(edges.size())-1;j>=0;j--){
				if(edges.get(j).first==frontIdx || edges.get(j).second==frontIdx){
					if(edges.get(j).first==frontIdx)	{temp=edges.get(j);}
					else							{temp.first=edges.get(j).second;temp.second=edges.get(j).first;}
					frontIdx=temp.second;
					front.add(temp);
					edges.set(j, edges.get(edges.size()-1));
					// edges.pop_back();
					edges.removeElementAt(edges.size()-1);
					j=(int)(edges.size());
				}
				else if(edges.get(j).first==backIdx || edges.get(j).second==backIdx){
					if(edges.get(j).second==backIdx)	{temp=edges.get(j);}
					else							{temp.first=edges.get(j).second;temp.second=edges.get(j).first;}
					backIdx=temp.first;
					back.add(temp);
					edges.set(j, edges.get(edges.size()-1));
					// edges.pop_back();
					edges.removeElementAt(edges.size()-1);
					j=(int)(edges.size());
				}
			}
			for(int j=(int)(back.size())-1;j>=0;j--){
				if ( loops.get(loopSize) == null ) {
				   loops.set(loopSize, new Vector<Pair<Long,Long> >());
				}
				loops.get(loopSize).add(back.get(j));
			}
			if ( loops.get(loopSize) == null ) {
			    loops.set(loopSize, new Vector<Pair<Long,Long> >());
			}
			loops.get(loopSize).add(e);
			for(int j=0;j<(int)(front.size());j++){
				if ( loops.get(loopSize) == null ) {
					   loops.set(loopSize, new Vector<Pair<Long,Long> >());
				}
				loops.get(loopSize).add(front.get(j));
			}
			loopSize++;
		}
		return (int)(loops.size());
	}
	
	public static int AddTriangles(CoredMeshData mesh,Vector<CoredPointIndex>[] edges, Vector<Point3D > interiorPositions, final int offSet){
		Vector<CoredPointIndex> e = new Vector<CoredPointIndex>();
		for(int i=0;i<3;i++){for(int j=0;j<edges[i].size();j++){e.add(edges[i].get(j));}}
		return AddTriangles(mesh,e,interiorPositions,offSet);
	}
	
	public static int AddTriangles(CoredMeshData mesh,Vector<CoredPointIndex> edges,Vector<Point3D> interiorPositions, final int offSet){
		if(edges.size()>3){
			Triangulation t = new Triangulation();

			// Add the points to the triangulation
			for(int i=0;i<(int)(edges.size());i++){
				Point3D p = new Point3D();
				if(edges.get(i).inCore == 1)	{
					// System.err.print("incore: ");
					for(int j=0;j<3;j++){
						p.coords[j]=mesh.inCorePoints.get(edges.get(i).index).coords[j];
						// System.err.print("      p.coords[" + j + "] = " + p.coords[j]);
					}
					// System.err.println();
					// pause();
				}
				else {
					// System.err.print("interior: ");
					for(int j=0;j<3;j++){
						p.coords[j]= interiorPositions.get(edges.get(i).index-offSet).coords[j];
						// System.err.print("      p.coords[" + j + "] = " + p.coords[j]);
					}
					// System.err.println();
				}
				
				t.points.add(p);
			}

			// Create a fan triangulation
			for(int i=1;i<(int)(edges.size())-1;i++){
				t.addTriangle(0,i,i+1);
			}

			// Minimize
			while(true){
				int i;
				for(i=0;i<(int)(t.edges.size());i++){if(t.flipMinimize(i) != 0){break;}}
				if(i==t.edges.size()){break;}
			}
			// Add the triangles to the mesh
			for(int i=0;i<(int)(t.triangles.size());i++){
				TriangleIndex tri = new TriangleIndex();
				int[] idx = new int[3];
				int[] idx0 = new int[1];
				int[] idx1 = new int[1];
				int[] idx2 = new int[1];
				int inCoreFlag=0;
				t.factor(i,idx0,idx1,idx2);
				
				idx[0] = idx0[0];
				idx[1] = idx1[0];
				idx[2] = idx2[0];
				
				for(int j=0;j<3;j++){
					tri.idx[j]=edges.get(idx[j]).index;
					if(edges.get(idx[j]).inCore == 1){
						inCoreFlag|=CoredVectorMeshData.IN_CORE_FLAG[j];}
				}
				mesh.addTriangle(tri,inCoreFlag);
			}
		}
		else if(edges.size()==3){
			TriangleIndex tri = new TriangleIndex();
			int inCoreFlag=0;
			for(int i=0;i<3;i++){
				tri.idx[i]=edges.get(i).index;
				if(edges.get(i).inCore == 1){inCoreFlag|=CoredVectorMeshData.IN_CORE_FLAG[i];}
			}
			mesh.addTriangle(tri,inCoreFlag);
		}
		return (int)(edges.size())-2;
	}
	
	
}
