package gov.nih.mipav.model.algorithms;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import gov.nih.mipav.model.algorithms.CeresSolver.Cell;
import gov.nih.mipav.model.algorithms.CeresSolver.CompressedRowBlockStructure;
import gov.nih.mipav.model.algorithms.CeresSolver.Pair;
import gov.nih.mipav.model.algorithms.CeresSolver.WeightedGraph;

/**
 * This is a port of the C++ files in ceres-solver-1.14.0 under the BSD license:
 * Ceres Solver - A fast non-linear least squares minimizer Copyright 2015
 * Google Inc. All rights reserved. http://ceres-solver.org/
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer. Redistributions in binary
 * form must reproduce the above copyright notice, this list of conditions and
 * the following disclaimer in the documentation and/or other materials provided
 * with the distribution. Neither the name of Google Inc. nor the names of its
 * contributors may be used to endorse or promote products derived from this
 * software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * Ceres Solver is an open source C++ library for modeling and solving large,
 * complicated optimization problems. It is a feature rich, mature and
 * performant library which has been used in production at Google since 2010.
 * Ceres Solver can solve two kinds of problems.
 * 
 * 1. Non-linear Least Squares problems with bounds constraints. 2. General
 * unconstrained optimization problems.
 * 
 * Please see [ceres-solver.org](http://ceres-solver.org/) for more information.
 * 
 * 
 * 
 * @author aailb
 *
 */

public class CeresSolver2 {
	CeresSolver ce = new CeresSolver();
	
	// This routine takes an array of integer values, sorts and uniques
			// them and then maps each value in the array to its position in the
			// sorted+uniqued array. By doing this, if there are are k unique
			// values in the array, each value is replaced by an integer in the
			// range [0, k-1], while preserving their relative order.
			//
			// For example
			//
			// [1 0 3 5 0 1 5]
			//
			// gets mapped to
			//
			// [1 0 2 3 0 1 3]
			public void MapValuesToContiguousRange(int[] array) {
				int i, j;
				int array_sort[] = new int[array.length];
				for (i = 0; i < array.length; i++) {
					array_sort[i] = array[i];
				}
				Arrays.sort(array_sort);
				int uniqueNum = 1;
				for (i = 1; i < array.length; i++) {
					if (array_sort[i] != array_sort[i-1]) {
						uniqueNum++;
					}
				}
				int uniqueArray[] = new int[uniqueNum];
				uniqueArray[0] = array_sort[0];
				for (i = 1, j = 1; i < array.length; i++) {
					if (array_sort[i] != array_sort[i-1]) {
						uniqueArray[j++] = array_sort[i];
					}
				}
				for (i = 0; i < uniqueNum; i++) {
					for (j = 0; j < array.length; j++) {
						if (array[j] == uniqueArray[i]) {
						     array[j] = i;	
						}
					}
				}
			}
			
			public double norm(double[] arg) {
				int i;
				double normSquared = 0.0;
				for (i = 0; i < arg.length; i++) {
					normSquared += arg[i] * arg[i];
				}
				return Math.sqrt(normSquared);
			}
			
			public void AngleAxisToQuaternion(double[] angle_axis, double[] quaternion) {
	  		  final double a0 = angle_axis[0];
	  		  final double a1 = angle_axis[1];
	  		  final double a2 = angle_axis[2];
	  		  final double theta_squared = a0 * a0 + a1 * a1 + a2 * a2;

	  		  // For points not at the origin, the full conversion is numerically stable.
	  		  if (theta_squared > 0.0) {
	  		    final double theta = Math.sqrt(theta_squared);
	  		    final double half_theta = theta * 0.5;
	  		    final double k = Math.sin(half_theta) / theta;
	  		    quaternion[0] = Math.cos(half_theta);
	  		    quaternion[1] = a0 * k;
	  		    quaternion[2] = a1 * k;
	  		    quaternion[3] = a2 * k;
	  		  } else {
	  		    // At the origin, sqrt() will produce NaN in the derivative since
	  		    // the argument is zero.  By approximating with a Taylor series,
	  		    // and truncating at one term, the value and first derivatives will be
	  		    // computed correctly when Jets are used.
	  		    double k = 0.5;
	  		    quaternion[0] = 1.0;
	  		    quaternion[1] = a0 * k;
	  		    quaternion[2] = a1 * k;
	  		    quaternion[3] = a2 * k;
	  		  }
	  		}

			public void QuaternionToAngleAxis(double[] quaternion, double[] angle_axis) {
				  final double q1 = quaternion[1];
				  final double q2 = quaternion[2];
				  final double q3 = quaternion[3];
				  final double sin_squared_theta = q1 * q1 + q2 * q2 + q3 * q3;

				  // For quaternions representing non-zero rotation, the conversion
				  // is numerically stable.
				  if (sin_squared_theta > 0.0) {
				    final double sin_theta = Math.sqrt(sin_squared_theta);
				    final double cos_theta = quaternion[0];

				    // If cos_theta is negative, theta is greater than pi/2, which
				    // means that angle for the angle_axis vector which is 2 * theta
				    // would be greater than pi.
				    //
				    // While this will result in the correct rotation, it does not
				    // result in a normalized angle-axis vector.
				    //
				    // In that case we observe that 2 * theta ~ 2 * theta - 2 * pi,
				    // which is equivalent saying
				    //
				    //   theta - pi = atan(sin(theta - pi), cos(theta - pi))
				    //              = atan(-sin(theta), -cos(theta))
				    //
				    final double two_theta =
				        2.0 * ((cos_theta < 0.0)
				                  ? Math.atan2(-sin_theta, -cos_theta)
				                  : Math.atan2(sin_theta, cos_theta));
				    final double k = two_theta / sin_theta;
				    angle_axis[0] = q1 * k;
				    angle_axis[1] = q2 * k;
				    angle_axis[2] = q3 * k;
				  } else {
				    // For zero rotation, sqrt() will produce NaN in the derivative since
				    // the argument is zero.  By approximating with a Taylor series,
				    // and truncating at one term, the value and first derivatives will be
				    // computed correctly when Jets are used.
				    final double k = 2.0;
				    angle_axis[0] = q1 * k;
				    angle_axis[1] = q2 * k;
				    angle_axis[2] = q3 * k;
				  }
				}
			
			public double DotProduct(double a[], double b[]) {
				int i;
				if (a.length != b.length) {
					System.err.println("a.length != b.length in DotProduct");
					return Double.NaN;
				}
				double result = 0.0;
				for (i = 0; i < a.length; i++) {
					result += a[i] * b[i];
				}
				return result;
			}
			
			//template <typename T>
			//inline void AngleAxisToRotationMatrix(const T* angle_axis, T* R) {
			  //AngleAxisToRotationMatrix(angle_axis, ColumnMajorAdapter3x3(R));
			//}

			//template <typename T, int row_stride, int col_stride>
			//void AngleAxisToRotationMatrix(
			    //const T* angle_axis,
			    //const MatrixAdapter<T, row_stride, col_stride>& R) {
			  // Put double R[] values in column major order
			public void AngleAxisToRotationMatrix(double[] angle_axis, double[] R) {
			  final double kOne = 1.0;
			  final double theta2 = DotProduct(angle_axis, angle_axis);
			  if (theta2 > ce.epsilon) {
			    // We want to be careful to only evaluate the square root if the
			    // norm of the angle_axis vector is greater than zero. Otherwise
			    // we get a division by zero.
			    final double theta = Math.sqrt(theta2);
			    final double wx = angle_axis[0] / theta;
			    final double wy = angle_axis[1] / theta;
			    final double wz = angle_axis[2] / theta;

			    final double costheta = Math.cos(theta);
			    final double sintheta = Math.sin(theta);

			    R[0] =     costheta   + wx*wx*(kOne -    costheta); // R(0, 0)
			    R[1] =  wz*sintheta   + wx*wy*(kOne -    costheta); // R(1, 0)
			    R[2] = -wy*sintheta   + wx*wz*(kOne -    costheta); // R(2, 0)
			    R[3] =  wx*wy*(kOne - costheta)     - wz*sintheta;  // R(0, 1)
			    R[4] =     costheta   + wy*wy*(kOne -    costheta); // R(1, 1)
			    R[5] =  wx*sintheta   + wy*wz*(kOne -    costheta); // R(2, 1)
			    R[6] =  wy*sintheta   + wx*wz*(kOne -    costheta); // R(0, 2)
			    R[7] = -wx*sintheta   + wy*wz*(kOne -    costheta); // R(1, 2)
			    R[8] =     costheta   + wz*wz*(kOne -    costheta); // R(2, 2)
			  } else {
			    // Near zero, we switch to using the first order Taylor expansion.
			    R[0] =  kOne; // R(0, 0)
			    R[1] =  angle_axis[2]; // R(1, 0)
			    R[2] = -angle_axis[1]; // R(2, 0)
			    R[3] = -angle_axis[2]; // R(0, 1)
			    R[4] =  kOne; // R(1, 1)
			    R[5] =  angle_axis[0]; // R(2, 1)
			    R[6] =  angle_axis[1]; // R(0, 2)
			    R[7] = -angle_axis[0]; // R(1 ,2)
			    R[8] = kOne; // R(2, 2)
			  }
			}
			
			//template <typename T>
			//void RotationMatrixToQuaternion(const T* R, T* angle_axis) {
			  //RotationMatrixToQuaternion(ColumnMajorAdapter3x3(R), angle_axis);
			//}

			// This algorithm comes from "Quaternion Calculus and Fast Animation",
			// Ken Shoemake, 1987 SIGGRAPH course notes
			//template <typename T, int row_stride, int col_stride>
			//void RotationMatrixToQuaternion(
			    //const MatrixAdapter<const T, row_stride, col_stride>& R,
			    //T* quaternion) {
			public void RotationMatrixToQuaternion(double R[], double quaternion[]) {
			  final double trace = R[0] + R[4] + R[8];
			  if (trace >= 0.0) {
			    double t = Math.sqrt(trace + 1.0);
			    quaternion[0] = 0.5 * t;
			    t = 0.5 / t;
			    quaternion[1] = (R[5] - R[7]) * t;
			    quaternion[2] = (R[6] - R[2]) * t;
			    quaternion[3] = (R[1] - R[3]) * t;
			  } else {
			    int i = 0;
			    if (R[4] > R[0]) {
			      i = 1;
			    }

			    if (R[8] > R[4*i]) {
			      i = 2;
			    }

			    final int j = (i + 1) % 3;
			    final int k = (j + 1) % 3;
			    double t = Math.sqrt(R[4*i] - R[4*j] - R[4*k] + 1.0);
			    quaternion[i + 1] = 0.5 * t;
			    t = 0.5 / t;
			    quaternion[0] = (R[k + 3*j] - R[j + 3*k]) * t;
			    quaternion[j + 1] = (R[j + 3*i] + R[i + 3*j]) * t;
			    quaternion[k + 1] = (R[k + 3*i] + R[i + 3*k]) * t;
			  }
			}
			
			// The conversion of a rotation matrix to the angle-axis form is
			// numerically problematic when then rotation angle is close to zero
			// or to Pi. The following implementation detects when these two cases
			// occurs and deals with them by taking code paths that are guaranteed
			// to not perform division by a small number.
			//template <typename T>
			//inline void RotationMatrixToAngleAxis(const T* R, T* angle_axis) {
			  //RotationMatrixToAngleAxis(ColumnMajorAdapter3x3(R), angle_axis);
			//}

			//template <typename T, int row_stride, int col_stride>
			//void RotationMatrixToAngleAxis(
			    //const MatrixAdapter<const T, row_stride, col_stride>& R,
			    //T* angle_axis) {
			public void RotationMatrixToAngleAxis(double R[], double angle_axis[]) {
			  double quaternion[] = new double[4];
			  RotationMatrixToQuaternion(R, quaternion);
			  QuaternionToAngleAxis(quaternion, angle_axis);
			  return;
			}
			
			// Transposes a 3x3 matrix.
			public void Transpose3x3(double m[]) {
			  double temp;
			  temp = m[1];
			  m[1] = m[3];
			  m[3] = temp;
			  temp = m[2];
			  m[2] = m[6];
			  m[6] = temp;
			  temp = m[5];
			  m[5] = m[7];
			  m[7] = temp;
			}
			
			// Convert Euler angles from radians to degrees.
			public void ToDegrees(double euler_angles[]) {
			  double scale = 180.0/Math.PI;
			  for (int i = 0; i < 3; ++i) {
			    euler_angles[i] *= scale;
			  }
			}

			//template <typename T>
			//inline void EulerAnglesToRotationMatrix(const T* euler,
			                                        //const int row_stride_parameter,
			                                        //T* R) {
			  //EulerAnglesToRotationMatrix(euler, RowMajorAdapter3x3(R));
			//}

			//template <typename T, int row_stride, int col_stride>
			//void EulerAnglesToRotationMatrix(
			    //const T* euler,
			    //const MatrixAdapter<T, row_stride, col_stride>& R) {
			public void EulerAnglesToRotationMatrix(double euler[], double R[]) {
			  final double degrees_to_radians = Math.PI / 180.0;

			  final double pitch = euler[0] * degrees_to_radians;
			  final double roll = euler[1] * degrees_to_radians;
			  final double yaw = euler[2] * degrees_to_radians;

			  final double c1 = Math.cos(yaw);
			  final double s1 = Math.sin(yaw);
			  final double c2 = Math.cos(roll);
			  final double s2 = Math.sin(roll);
			  final double c3 = Math.cos(pitch);
			  final double s3 = Math.sin(pitch);

			  R[0] = c1*c2; // R(0, 0)
			  R[1] = -s1*c3 + c1*s2*s3; // R(0, 1)
			  R[2] = s1*s3 + c1*s2*c3; // R(0, 2)

			  R[3] = s1*c2; // R(1, 0)
			  R[4] = c1*c3 + s1*s2*s3; // R(1, 1)
			  R[5] = -c1*s3 + s1*s2*c3; // R(1, 2)

			  R[6] = -s2; // R(2, 0)
			  R[7] = c2*s3; // R(2, 1)
			  R[8] = c2*c3; // R(2, 2)
			}
			
			//template <typename T> inline
			//void QuaternionToScaledRotation(const T q[4], T R[3 * 3]) {
			  //QuaternionToScaledRotation(q, RowMajorAdapter3x3(R));
			//}

			//template <typename T, int row_stride, int col_stride> inline
			//void QuaternionToScaledRotation(
			    //const T q[4],
			    //const MatrixAdapter<T, row_stride, col_stride>& R) {
			  // Make convenient names for elements of q.
			public void QuaternionToScaledRotation(double q[], double R[]) {
			  double a = q[0];
			  double b = q[1];
			  double c = q[2];
			  double d = q[3];
			  // This is not to eliminate common sub-expression, but to
			  // make the lines shorter so that they fit in 80 columns!
			  double aa = a * a;
			  double ab = a * b;
			  double ac = a * c;
			  double ad = a * d;
			  double bb = b * b;
			  double bc = b * c;
			  double bd = b * d;
			  double cc = c * c;
			  double cd = c * d;
			  double dd = d * d;

			  R[0] = aa + bb - cc - dd; R[1] = 2 * (bc - ad);  R[2] = 2 * (ac + bd);
			  R[3] = 2 * (ad + bc);  R[4] = aa - bb + cc - dd; R[5] = 2 * (cd - ab);
			  R[6] = 2 * (bd - ac);  R[7] = 2 * (ab + cd);  R[8] = aa - bb - cc + dd;
			}
			
			//template <typename T> inline
			//void QuaternionToRotation(const T q[4], T R[3 * 3]) {
			  //QuaternionToRotation(q, RowMajorAdapter3x3(R));
			//}

			//template <typename T, int row_stride, int col_stride> inline
			//void QuaternionToRotation(const T q[4],
			                          //const MatrixAdapter<T, row_stride, col_stride>& R) {
			public void QuaternionToRotation(double q[], double R[]) {
			  QuaternionToScaledRotation(q, R);

			  double normalizer = q[0]*q[0] + q[1]*q[1] + q[2]*q[2] + q[3]*q[3];
			  normalizer = 1.0 / normalizer;

			  for (int i = 0; i < 3; ++i) {
			    for (int j = 0; j < 3; ++j) {
			      R[3 * i + j] *= normalizer;
			    }
			  }
			}
			
			//template <typename T> inline
			//void UnitQuaternionRotatePoint(const T q[4], const T pt[3], T result[3]) {
			public void UnitQuaternionRotatePoint(double q[], double pt[], double result[]) {
			  final double t2 =  q[0] * q[1];
			  final double t3 =  q[0] * q[2];
			  final double t4 =  q[0] * q[3];
			  final double t5 = -q[1] * q[1];
			  final double t6 =  q[1] * q[2];
			  final double t7 =  q[1] * q[3];
			  final double t8 = -q[2] * q[2];
			  final double t9 =  q[2] * q[3];
			  final double t1 = -q[3] * q[3];
			  result[0] = 2 * ((t8 + t1) * pt[0] + (t6 - t4) * pt[1] + (t3 + t7) * pt[2]) + pt[0];
			  result[1] = 2 * ((t4 + t6) * pt[0] + (t5 + t1) * pt[1] + (t9 - t2) * pt[2]) + pt[1];
			  result[2] = 2 * ((t7 - t3) * pt[0] + (t2 + t9) * pt[1] + (t5 + t8) * pt[2]) + pt[2];
			}
			
			//template<typename T> inline
			//void AngleAxisRotatePoint(const T angle_axis[3], const T pt[3], T result[3]) {
			public void AngleAxisRotatePoint(double angle_axis[], double pt[], double result[]) {
			  final double theta2 = DotProduct(angle_axis, angle_axis);
			  if (theta2 > ce.epsilon) {
			    // Away from zero, use the rodriguez formula
			    //
			    //   result = pt costheta +
			    //            (w x pt) * sintheta +
			    //            w (w . pt) (1 - costheta)
			    //
			    // We want to be careful to only evaluate the square root if the
			    // norm of the angle_axis vector is greater than zero. Otherwise
			    // we get a division by zero.
			    //
			    final double theta = Math.sqrt(theta2);
			    final double costheta = Math.cos(theta);
			    final double sintheta = Math.sin(theta);
			    final double theta_inverse = 1.0 / theta;

			    final double w[] = new double[] { angle_axis[0] * theta_inverse,
			                     angle_axis[1] * theta_inverse,
			                     angle_axis[2] * theta_inverse };

			    // Explicitly inlined evaluation of the cross product for
			    // performance reasons.
			    final double w_cross_pt[] = new double[] { w[1] * pt[2] - w[2] * pt[1],
			                              w[2] * pt[0] - w[0] * pt[2],
			                              w[0] * pt[1] - w[1] * pt[0] };
			    final double tmp =
			        (w[0] * pt[0] + w[1] * pt[1] + w[2] * pt[2]) * (1.0 - costheta);

			    result[0] = pt[0] * costheta + w_cross_pt[0] * sintheta + w[0] * tmp;
			    result[1] = pt[1] * costheta + w_cross_pt[1] * sintheta + w[1] * tmp;
			    result[2] = pt[2] * costheta + w_cross_pt[2] * sintheta + w[2] * tmp;
			  } else {
			    // Near zero, the first order Taylor approximation of the rotation
			    // matrix R corresponding to a vector w and angle w is
			    //
			    //   R = I + hat(w) * sin(theta)
			    //
			    // But sintheta ~ theta and theta * w = angle_axis, which gives us
			    //
			    //  R = I + hat(w)
			    //
			    // and actually performing multiplication with the point pt, gives us
			    // R * pt = pt + w x pt.
			    //
			    // Switching to the Taylor expansion near zero provides meaningful
			    // derivatives when evaluated using Jets.
			    //
			    // Explicitly inlined evaluation of the cross product for
			    // performance reasons.
			    final double w_cross_pt[] = new double[] { angle_axis[1] * pt[2] - angle_axis[2] * pt[1],
			                              angle_axis[2] * pt[0] - angle_axis[0] * pt[2],
			                              angle_axis[0] * pt[1] - angle_axis[1] * pt[0] };

			    result[0] = pt[0] + w_cross_pt[0];
			    result[1] = pt[1] + w_cross_pt[1];
			    result[2] = pt[2] + w_cross_pt[2];
			  }
			}
			
			public void ComputeVisibility(CompressedRowBlockStructure block_structure,
	                int num_eliminate_blocks,
	                Vector<HashSet<Integer>> visibility) {
				int i;
			    if (visibility == null) {
			    	System.err.println("In ComputeVisibility visibility == null");
			    	return;
			    }
				
				// Clear the visibility vector and resize it to hold a
				// vector for each camera.
				visibility.clear();
				int visSize = block_structure.cols.size() - num_eliminate_blocks;
				for (i = 0; i < visSize; i++) {
					visibility.add(new HashSet<Integer>());
				}
				
				for (i = 0; i < block_structure.rows.size(); ++i) {
				final Vector<Cell> cells = block_structure.rows.get(i).cells;
				int block_id = cells.get(0).block_id;
				// If the first block is not an e_block, then skip this row block.
				if (block_id >= num_eliminate_blocks) {
				continue;
				}
				
				for (int j = 1; j < cells.size(); ++j) {
				int camera_block_id = cells.get(j).block_id - num_eliminate_blocks;
				if(camera_block_id < 0) {
					System.err.println("In ComputeVisibility camera_block_id < 0");
					return;
				}
				if (camera_block_id >=visibility.size()) {
					System.err.println("In ComputeVisibilty camera_block_size >= visibility.size()");
					return;
				}
				visibility.get(camera_block_id).add(block_id);
				}
				}
			}
			
			WeightedGraph<Integer> CreateSchurComplementGraph(
				    Vector<HashSet<Integer> > visibility) {
				  int i,j;
				  int camera1, camera2;
				  // Compute the number of e_blocks/point blocks. Since the visibility
				  // set for each e_block/camera contains the set of e_blocks/points
				  // visible to it, we find the maximum across all visibility sets.
				  int num_points = 0;
				  for (i = 0; i < visibility.size(); i++) {
				    if (visibility.get(i).size() > 0) {
				    	int lastValue = 0;
				    	final HashSet<Integer> visibility_set = visibility.get(i);
					    Iterator<Integer> visibility_it = visibility_set.iterator();
					    while (visibility_it.hasNext()) {
					    	lastValue = visibility_it.next();
					    }
				      num_points = Math.max(num_points, lastValue+1);
				    }
				  }

				  // Invert the visibility. The input is a camera->point mapping,
				  // which tells us which points are visible in which
				  // cameras. However, to compute the sparsity structure of the Schur
				  // Complement efficiently, its better to have the point->camera
				  // mapping.
				  Vector<HashSet<Integer> > inverse_visibility = new Vector<HashSet<Integer>>(num_points);
				  for (i = 0; i < num_points; i++) {
					  inverse_visibility.add(new HashSet<Integer>());
				  }
				  
				  for (i = 0; i < visibility.size(); i++) {
				    final HashSet<Integer> visibility_set = visibility.get(i);
				    Iterator<Integer> visibility_it = visibility_set.iterator();
				    while (visibility_it.hasNext()) {
				    	inverse_visibility.get(visibility_it.next()).add(i);
				    }
				  }

				  // Map from camera pairs to number of points visible to both cameras
				  // in the pair.
				  HashMap<Pair<Integer, Integer>, Integer > camera_pairs = new HashMap<Pair<Integer, Integer>, Integer>();

				  // Count the number of points visible to each camera/f_block pair.
				  for (i = 0; i < inverse_visibility.size(); i++) {
				    final HashSet<Integer> inverse_visibility_set = inverse_visibility.get(i);
				    Iterator<Integer> camera1_it = inverse_visibility_set.iterator();
				    int numTimes = 0;
				    while (camera1_it.hasNext()) {
				    	numTimes++;
				    	camera1 = camera1_it.next();
				    	Iterator<Integer> camera2_it = inverse_visibility_set.iterator();
				    	for (j = 0; j < numTimes; j++) {
				    		camera2_it.next();
				    	}
				        while (camera2_it.hasNext()) {
				        	camera2 = camera2_it.next();
					        Pair<Integer, Integer> pair = ce.new Pair<Integer, Integer>(camera1, camera2);
					        if (camera_pairs.get(pair) == null) {
					        	camera_pairs.put(pair, 1);
					        }
					        else {
					        	int oldValue = camera_pairs.get(pair);
					        	int newValue = oldValue +1;
					        	camera_pairs.replace(pair, oldValue, newValue);
					        }
				      }
				    }
				  }

				  WeightedGraph<Integer> graph = ce.new WeightedGraph<Integer>();

				  // Add vertices and initialize the pairs for self edges so that self
				  // edges are guaranteed. This is needed for the Canonical views
				  // algorithm to work correctly.
				  final double kSelfEdgeWeight = 1.0;
				  for (i = 0; i < visibility.size(); ++i) {
				    graph.AddVertex(i);
				    graph.AddEdge(i, i, kSelfEdgeWeight);
				  }

				  // Add an edge for each camera pair.
				    Collection<Integer> intValues = camera_pairs.values();
					Iterator<Integer> intValues_it = intValues.iterator();
					Set<Pair<Integer, Integer>> pairSet = camera_pairs.keySet();
					Iterator<Pair<Integer, Integer>> pair_iterator = pairSet.iterator();
					while (pair_iterator.hasNext()) {
						Pair<Integer, Integer> pair = pair_iterator.next();
						int count = intValues_it.next();
						camera1 = pair.first;
				        camera2 = pair.second;
				        if (camera1 == camera2) {
				        	System.err.println("In CreateSchurComplementGraph camera1 == camera2");
				        	return null;
				        }

				    // Static cast necessary for Windows.
				    final double weight = (double)(count) /
				        (Math.sqrt((double)(
				                  visibility.get(camera1).size() * visibility.get(camera2).size())));
				    graph.AddEdge(camera1, camera2, weight);
				  }

				  return graph;
				}
}