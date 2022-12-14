package gov.nih.mipav.model.algorithms;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import Jama.Matrix;

import java.util.Map.Entry;

import gov.nih.mipav.model.algorithms.CeresSolver.BlockRandomAccessMatrix;
import gov.nih.mipav.model.algorithms.CeresSolver.CRSMatrix;
import gov.nih.mipav.model.algorithms.CeresSolver.Cell;
import gov.nih.mipav.model.algorithms.CeresSolver.CompressedRowBlockStructure;
import gov.nih.mipav.model.algorithms.CeresSolver.CostFunction;
import gov.nih.mipav.model.algorithms.CeresSolver.CovarianceAlgorithmType;
import gov.nih.mipav.model.algorithms.CeresSolver.EvaluateOptions;
import gov.nih.mipav.model.algorithms.CeresSolver.EventLogger;
import gov.nih.mipav.model.algorithms.CeresSolver.Ownership;
import gov.nih.mipav.model.algorithms.CeresSolver.Pair;
import gov.nih.mipav.model.algorithms.CeresSolver.ParameterBlock;
import gov.nih.mipav.model.algorithms.CeresSolver.ProblemImpl;
import gov.nih.mipav.model.algorithms.CeresSolver.ResidualBlock;
import gov.nih.mipav.model.algorithms.CeresSolver.SparseLinearAlgebraLibraryType;
import gov.nih.mipav.model.algorithms.CeresSolver.SparseMatrix;
import gov.nih.mipav.model.algorithms.CeresSolver.TripletSparseMatrix;
import gov.nih.mipav.model.algorithms.CeresSolver.WeightedGraph;
import gov.nih.mipav.model.algorithms.CeresSolver.CellInfo;
import gov.nih.mipav.view.Preferences;

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

public class CeresSolver2 extends CeresSolver {
	public boolean fitToExternalFunction(double x[], double residuals[], double jacobian[][]) {
		return true;
	}
	
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
		  if (theta2 > epsilon) {
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
		  if (theta2 > epsilon) {
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
			        Pair<Integer, Integer> pair = new Pair<Integer, Integer>(camera1, camera2);
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

		  WeightedGraph<Integer> graph = new WeightedGraph<Integer>();

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
		
		// An object that implements an infinite one dimensional grid needed
		// by the CubicInterpolator where the source of the function values is
		// an array of type T on the interval
		//
		//   [begin, ..., end - 1]
		//
		// Since the input array is finite and the grid is infinite, values
		// outside this interval needs to be computed. Grid1D uses the value
		// from the nearest edge.
		//
		// The function being provided can be vector valued, in which case
		// kDataDimension > 1. The dimensional slices of the function maybe
		// interleaved, or they maybe stacked, i.e, if the function has
		// kDataDimension = 2, if kInterleaved = true, then it is stored as
		//
		//   f01, f02, f11, f12 ....
		//
		// and if kInterleaved = false, then it is stored as
		//
		//  f01, f11, .. fn1, f02, f12, .. , fn2
		//
		//template <typename T,
		//          int kDataDimension = 1,
		//          bool kInterleaved = true>
	    public class Grid1D {
		  // class cast exception if try to go from Integer to double
		  //enum { DATA_DIMENSION = kDataDimension };
		private final double[] data_;
		private int DATA_DIMENSION = 1;
		private boolean kInterleaved = true;
		private int begin_;
	    private int end_;
	    private int num_values_;

		  public Grid1D(int[] x, int kDataDimension, boolean interleaved, int begin, int end) {
			  int i;
			  data_ = new double[x.length];
			  for (i = 0; i < x.length; i++) {
				  data_[i] = (double)(x[i]);
			  }
			  DATA_DIMENSION = kDataDimension;
			  kInterleaved = interleaved;
			  begin_ = begin;
			  end_ = end;
			  num_values_ = end - begin;
			  if (begin >= end) {
				  System.err.println("begin >= end in public Grid1D");
			  }
		  }
		  
		  public Grid1D(double[] x, int kDataDimension, boolean interleaved, int begin, int end) {
			  data_ = x;
			  DATA_DIMENSION = kDataDimension;
			  kInterleaved = interleaved;
			  begin_ = begin;
			  end_ = end;
			  num_values_ = end - begin;
			  if (begin >= end) {
				  System.err.println("begin >= end in public Grid1D");
			  }
		  }
		  
		  public int getDataDimension() {
			  return DATA_DIMENSION;
		  }

		public void GetValue(int n, double[] f) {
		    final int idx = Math.min(Math.max(begin_, n), end_ - 1) - begin_;
		    if (kInterleaved) {
		      for (int i = 0; i < DATA_DIMENSION; ++i) {
		        f[i] = data_[DATA_DIMENSION * idx + i];
		      }
		    } else {
		      for (int i = 0; i < DATA_DIMENSION; ++i) {
		        f[i] = data_[i * num_values_ + idx];
		      }
		    }
		  }

		 
		};
		
		// An object that implements an infinite two dimensional grid needed
		// by the BiCubicInterpolator where the source of the function values
		// is an grid of type T on the grid
		//
		//   [(row_start,   col_start), ..., (row_start,   col_end - 1)]
		//   [                          ...                            ]
		//   [(row_end - 1, col_start), ..., (row_end - 1, col_end - 1)]
		//
		// Since the input grid is finite and the grid is infinite, values
		// outside this interval needs to be computed. Grid2D uses the value
		// from the nearest edge.
		//
		// The function being provided can be vector valued, in which case
		// kDataDimension > 1. The data maybe stored in row or column major
		// format and the various dimensional slices of the function maybe
		// interleaved, or they maybe stacked, i.e, if the function has
		// kDataDimension = 2, is stored in row-major format and if
		// kInterleaved = true, then it is stored as
		//
		//   f001, f002, f011, f012, ...
		//
		// A commonly occuring example are color images (RGB) where the three
		// channels are stored interleaved.
		//
		// If kInterleaved = false, then it is stored as
		//
		//  f001, f011, ..., fnm1, f002, f012, ...
		//template <typename T,
		//          int kDataDimension = 1,
		//          bool kRowMajor = true,
		//          bool kInterleaved = true>
		public class Grid2D {
			private final double[] data_;
			private int DATA_DIMENSION = 1;
			private boolean kRowMajor = true;
			private boolean kInterleaved = true;
		    private final int row_begin_;
		    private final int row_end_;
		    private final int col_begin_;
			private final int col_end_;
			private final int num_rows_;
			private final int num_cols_;
		    private final int num_values_;
		 //public:
		  //enum { DATA_DIMENSION = kDataDimension };

		  public Grid2D(int[] data, int kDataDimension, boolean rowMajor, boolean interleaved,
		         int row_begin, int row_end,
		         int col_begin, int col_end) {
			  int i;
			  data_ = new double[data.length];
			  for (i = 0; i < data.length; i++) {
				  data_[i] = (double)(data[i]);
			  }
			  DATA_DIMENSION = kDataDimension;
			  kRowMajor = rowMajor;
			  kInterleaved = interleaved;
		      row_begin_ = row_begin;
		      row_end_ = row_end;
		      col_begin_ = col_begin;
		      col_end_ = col_end;
		      num_rows_ = row_end - row_begin;
		      num_cols_ = col_end - col_begin;
		      num_values_ = (num_rows_ * num_cols_);
		      if (kDataDimension < 1) {
		    	  System.err.println("In public Grid2D kDataDimension < 1");
		    	  return;
		      }
		      if (row_begin >= row_end) {
		    	  System.err.println("In public Grid2D row_begin >= row_end");
		    	  return;
		      }
		      if (col_begin >= col_end) {
		    	  System.err.println("In public Grid2D col_begin >= col_end");
		      }
		  }
		  
		  public Grid2D(double[] data, int kDataDimension, boolean rowMajor, boolean interleaved,
			         int row_begin, int row_end,
			         int col_begin, int col_end) {
				  data_ = data;
				  DATA_DIMENSION = kDataDimension;
				  kRowMajor = rowMajor;
				  kInterleaved = interleaved;
			      row_begin_ = row_begin;
			      row_end_ = row_end;
			      col_begin_ = col_begin;
			      col_end_ = col_end;
			      num_rows_ = row_end - row_begin;
			      num_cols_ = col_end - col_begin;
			      num_values_ = (num_rows_ * num_cols_);
			      if (kDataDimension < 1) {
			    	  System.err.println("In public Grid2D kDataDimension < 1");
			    	  return;
			      }
			      if (row_begin >= row_end) {
			    	  System.err.println("In public Grid2D row_begin >= row_end");
			    	  return;
			      }
			      if (col_begin >= col_end) {
			    	  System.err.println("In public Grid2D col_begin >= col_end");
			      }
			  }
		  
		  public int getDataDimension() {
			  return DATA_DIMENSION;
		  }

		  public void GetValue( int r, int c, double[] f) {
		    final int row_idx =
		        Math.min(Math.max(row_begin_, r), row_end_ - 1) - row_begin_;
		    final int col_idx =
		        Math.min(Math.max(col_begin_, c), col_end_ - 1) - col_begin_;

		    final int n =
		        (kRowMajor)
		        ? num_cols_ * row_idx + col_idx
		        : num_rows_ * col_idx + row_idx;


		    if (kInterleaved) {
		      for (int i = 0; i < DATA_DIMENSION; ++i) {
		        f[i] = data_[DATA_DIMENSION * n + i];
		      }
		    } else {
		      for (int i = 0; i < DATA_DIMENSION; ++i) {
		        f[i] = data_[i * num_values_ + n];
		      }
		    }
		  }

		 
		};
			
			
		// Given samples from a function sampled at four equally spaced points,
		//
		//   p0 = f(-1)
		//   p1 = f(0)
		//   p2 = f(1)
		//   p3 = f(2)
		//
		// Evaluate the cubic Hermite spline (also known as the Catmull-Rom
		// spline) at a point x that lies in the interval [0, 1].
		//
		// This is also the interpolation kernel (for the case of a = 0.5) as
		// proposed by R. Keys, in:
		//
		// "Cubic convolution interpolation for digital image processing".
		// IEEE Transactions on Acoustics, Speech, and Signal Processing
		// 29 (6): 1153-1160.
		//
		// For more details see
		//
		// http://en.wikipedia.org/wiki/Cubic_Hermite_spline
		// http://en.wikipedia.org/wiki/Bicubic_interpolation
		//
		// f if not NULL will contain the interpolated function values.
		// dfdx if not NULL will contain the interpolated derivative values.
		//template <int kDataDimension>
		public void CubicHermiteSpline(
				                int kDataDimension,
				                double p0[],
				                double p1[],
				                double p2[],
				                double p3[],
		                        double x,
		                        double[] f,
		                        double[] dfdx) {

		  double a[] = new double[kDataDimension];
		  double b[] = new double[kDataDimension];
		  double c[] = new double[kDataDimension];
		  int i;
		  for (i = 0; i < kDataDimension; i++) {
			  a[i] = 0.5 * (-p0[i] + 3.0 * p1[i] - 3.0 * p2[i] + p3[i]);
			  b[i] = 0.5 * (2.0 * p0[i] - 5.0 * p1[i] + 4.0 * p2[i] - p3[i]);
			  c[i] = 0.5 * (-p0[i] + p2[i]);
		  }

		  // Use Horner's rule to evaluate the function value and its
		  // derivative.

		  // f = ax^3 + bx^2 + cx + p1
		  if (f != null) {
			for (i = 0; i < kDataDimension; i++) {
				f[i] = p1[i] + x * (c[i] + x * (b[i] + x * a[i]));
			}
		  }

		  // dfdx = 3ax^2 + 2bx + c
		  if (dfdx != null) {
			for (i = 0; i < kDataDimension; i++) {
				dfdx[i] = c[i] + x * (2.0 * b[i] + 3.0 * a[i] * x);
			}
		  }
		}
			
		// Given as input an infinite one dimensional grid, which provides the
		// following interface.
		//
		//   class Grid {
		//    public:
		//     enum { DATA_DIMENSION = 2; };
		//     void GetValue(int n, double* f) const;
		//   };
		//
		// Here, GetValue gives the value of a function f (possibly vector
		// valued) for any integer n.
		//
		// The enum DATA_DIMENSION indicates the dimensionality of the
		// function being interpolated. For example if you are interpolating
		// rotations in axis-angle format over time, then DATA_DIMENSION = 3.
		//
		// CubicInterpolator uses cubic Hermite splines to produce a smooth
		// approximation to it that can be used to evaluate the f(x) and f'(x)
		// at any point on the real number line.
		//
		// For more details on cubic interpolation see
		//
		// http://en.wikipedia.org/wiki/Cubic_Hermite_spline
		//
		// Example usage:
		//
		//  const double data[] = {1.0, 2.0, 5.0, 6.0};
		//  Grid1D<double, 1> grid(x, 0, 4);
		//  CubicInterpolator<Grid1D<double, 1> > interpolator(grid);
		//  double f, dfdx;
		//  interpolator.Evaluator(1.5, &f, &dfdx);
		//template<typename Grid>
		public class CubicInterpolator {
			private Grid1D grid_;
			private int DATA_DIMENSION;
		 public CubicInterpolator(Grid1D grid) {
		      grid_ = grid;
		    // The + casts the enum into an int before doing the
		    // comparison. It is needed to prevent
		    // "-Wunnamed-type-template-args" related errors.
		    if (grid.getDataDimension() < 1) {
		    	System.err.println("grid.getDataDimension() < 1");
		    	return;
		    }
		    DATA_DIMENSION = grid.getDataDimension();
		  }

		  public void Evaluate(double x, double[] f, double[]dfdx) {
		    final int n = (int)Math.floor(x);
		    double p0[] = new double[DATA_DIMENSION];
		    double p1[] = new double[DATA_DIMENSION];
		    double p2[] = new double[DATA_DIMENSION];
		    double p3[] = new double[DATA_DIMENSION];
		    grid_.GetValue(n - 1, p0);
		    grid_.GetValue(n,     p1);
		    grid_.GetValue(n + 1, p2);
		    grid_.GetValue(n + 2, p3);
		    CubicHermiteSpline(DATA_DIMENSION, p0, p1, p2, p3, x - n, f, dfdx);
		  }

		  // The following two Evaluate overloads are needed for interfacing
		  // with automatic differentiation. The first is for when a scalar
		  // evaluation is done, and the second one is for when Jets are used.
		  void Evaluate(double x, double[] f) {
		    Evaluate(x, f, null);
		  }

		  /*template<typename JetT> void Evaluate(const JetT& x, JetT* f) const {
		    double fx[Grid::DATA_DIMENSION], dfdx[Grid::DATA_DIMENSION];
		    Evaluate(x.a, fx, dfdx);
		    for (int i = 0; i < Grid::DATA_DIMENSION; ++i) {
		      f[i].a = fx[i];
		      f[i].v = dfdx[i] * x.v;
		    }
		  }*/

		
		};
		
		// BiCubicInterpolator uses the cubic convolution interpolation
		// algorithm of R. Keys, to produce a smooth approximation to it that
		// can be used to evaluate the f(r,c), df(r, c)/dr and df(r,c)/dc at
		// any point in the real plane.
		//
		// For more details on the algorithm used here see:
		//
		// "Cubic convolution interpolation for digital image processing".
		// Robert G. Keys, IEEE Trans. on Acoustics, Speech, and Signal
		// Processing 29 (6): 1153-1160, 1981.
		//
		// http://en.wikipedia.org/wiki/Cubic_Hermite_spline
		// http://en.wikipedia.org/wiki/Bicubic_interpolation
		//
		// Example usage:
		//
		// const double data[] = {1.0, 3.0, -1.0, 4.0,
		//                        3.6, 2.1,  4.2, 2.0,
		//                        2.0, 1.0,  3.1, 5.2};
		//  Grid2D<double, 1>  grid(data, 3, 4);
		//  BiCubicInterpolator<Grid2D<double, 1> > interpolator(grid);
		//  double f, dfdr, dfdc;
		//  interpolator.Evaluate(1.2, 2.5, &f, &dfdr, &dfdc);

		//template<typename Grid>
		public class BiCubicInterpolator {
			private Grid2D grid_;
			private int DATA_DIMENSION;
		 public BiCubicInterpolator(Grid2D grid) {
		      grid_ = grid;
		    // The + casts the enum into an int before doing the
		    // comparison. It is needed to prevent
		    // "-Wunnamed-type-template-args" related errors.
		    if (grid.getDataDimension() < 1) {
		    	System.err.println("grid.getDataDimension() < 1");
		    	return;
		    }
		    DATA_DIMENSION = grid.getDataDimension();
		  }
		 

		  // Evaluate the interpolated function value and/or its
		  // derivative. Returns false if r or c is out of bounds.
		  public void Evaluate(double r, double c,
		                double[] f, double[] dfdr, double[] dfdc) {
		    // BiCubic interpolation requires 16 values around the point being
		    // evaluated.  We will use pij, to indicate the elements of the
		    // 4x4 grid of values.
		    //
		    //          col
		    //      p00 p01 p02 p03
		    // row  p10 p11 p12 p13
		    //      p20 p21 p22 p23
		    //      p30 p31 p32 p33
		    //
		    // The point (r,c) being evaluated is assumed to lie in the square
		    // defined by p11, p12, p22 and p21.

		    final int row = (int)Math.floor(r);
		    final int col = (int)Math.floor(c);

		    double p0[] = new double[DATA_DIMENSION];
		    double p1[] = new double[DATA_DIMENSION];
		    double p2[] = new double[DATA_DIMENSION];
		    double p3[] = new double[DATA_DIMENSION];

		    // Interpolate along each of the four rows, evaluating the function
		    // value and the horizontal derivative in each row.
		    double f0[] = new double[DATA_DIMENSION];
		    double f1[] = new double[DATA_DIMENSION];
		    double f2[] = new double[DATA_DIMENSION];
		    double f3[] = new double[DATA_DIMENSION];
		    double df0dc[] = new double[DATA_DIMENSION];
		    double df1dc[] = new double[DATA_DIMENSION];
		    double df2dc[] = new double[DATA_DIMENSION];
		    double df3dc[] = new double[DATA_DIMENSION];

		    grid_.GetValue(row - 1, col - 1, p0);
		    grid_.GetValue(row - 1, col    , p1);
		    grid_.GetValue(row - 1, col + 1, p2);
		    grid_.GetValue(row - 1, col + 2, p3);
		    CubicHermiteSpline(DATA_DIMENSION,p0, p1, p2, p3, c - col,
		                                             f0, df0dc);

		    grid_.GetValue(row, col - 1, p0);
		    grid_.GetValue(row, col    , p1);
		    grid_.GetValue(row, col + 1, p2);
		    grid_.GetValue(row, col + 2, p3);
		    CubicHermiteSpline(DATA_DIMENSION,p0, p1, p2, p3, c - col,
		                                             f1, df1dc);

		    grid_.GetValue(row + 1, col - 1, p0);
		    grid_.GetValue(row + 1, col    , p1);
		    grid_.GetValue(row + 1, col + 1, p2);
		    grid_.GetValue(row + 1, col + 2, p3);
		    CubicHermiteSpline(DATA_DIMENSION,p0, p1, p2, p3, c - col,
		                                             f2, df2dc);

		    grid_.GetValue(row + 2, col - 1, p0);
		    grid_.GetValue(row + 2, col    , p1);
		    grid_.GetValue(row + 2, col + 1, p2);
		    grid_.GetValue(row + 2, col + 2, p3);
		    CubicHermiteSpline(DATA_DIMENSION,p0, p1, p2, p3, c - col,
		                                             f3, df3dc);

		    // Interpolate vertically the interpolated value from each row and
		    // compute the derivative along the columns.
		    CubicHermiteSpline(DATA_DIMENSION,f0, f1, f2, f3, r - row, f, dfdr);
		    if (dfdc != null) {
		      // Interpolate vertically the derivative along the columns.
		      CubicHermiteSpline(DATA_DIMENSION,df0dc, df1dc, df2dc, df3dc,
		                                               r - row, dfdc, null);
		    }
		  }

		  // The following two Evaluate overloads are needed for interfacing
		  // with automatic differentiation. The first is for when a scalar
		  // evaluation is done, and the second one is for when Jets are used.
		  public void Evaluate(double r, double c, double[] f) {
		    Evaluate(r, c, f, null, null);
		  }

		  /*template<typename JetT> void Evaluate(const JetT& r,
		                                        const JetT& c,
		                                        JetT* f) const {
		    double frc[Grid::DATA_DIMENSION];
		    double dfdr[Grid::DATA_DIMENSION];
		    double dfdc[Grid::DATA_DIMENSION];
		    Evaluate(r.a, c.a, frc, dfdr, dfdc);
		    for (int i = 0; i < Grid::DATA_DIMENSION; ++i) {
		      f[i].a = frc[i];
		      f[i].v = dfdr[i] * r.v + dfdc[i] * c.v;
		    }
		  }*/

		 
		};
			
		public class CanonicalViewsClusteringOptions {
			// The minimum number of canonical views to compute.
			  public int min_views;

			  // Penalty weight for the number of canonical views.  A higher
			  // number will result in fewer canonical views.
			  public double size_penalty_weight;

			  // Penalty weight for the diversity (orthogonality) of the
			  // canonical views.  A higher number will encourage less similar
			  // canonical views.
			  public double similarity_penalty_weight;

			  // Weight for per-view scores.  Lower weight places less
			  // confidence in the view scores.
			  public double view_score_weight;
			  public CanonicalViewsClusteringOptions() {
			        min_views = 3;
			        size_penalty_weight = 5.75;
			        similarity_penalty_weight = 100.0;
			        view_score_weight = 0.0;
			  }
			  
		}
			
		class CanonicalViewsClustering {
			CanonicalViewsClusteringOptions options_;
			  WeightedGraph<Integer> graph_;
			  // Maps a view to its representative canonical view (its cluster
			  // center).
			  HashMap<Integer, Integer> view_to_canonical_view_;
			  // Maps a view to its similarity to its current cluster center.
			  HashMap<Integer, Double> view_to_canonical_view_similarity_;
			 public CanonicalViewsClustering() {
				 options_ = new CanonicalViewsClusteringOptions();
				 graph_ = new WeightedGraph<Integer>();
				 view_to_canonical_view_ = new HashMap<Integer, Integer>();
				 view_to_canonical_view_similarity_ = new HashMap<Integer, Double>();
			 }

			  // Compute the canonical views clustering of the vertices of the
			  // graph. centers will contain the vertices that are the identified
			  // as the canonical views/cluster centers, and membership is a map
			  // from vertices to cluster_ids. The i^th cluster center corresponds
			  // to the i^th cluster. It is possible depending on the
			  // configuration of the clustering algorithm that some of the
			  // vertices may not be assigned to any cluster. In this case they
			  // are assigned to a cluster with id = kInvalidClusterId.
			  public void ComputeClustering(CanonicalViewsClusteringOptions options,
			                                WeightedGraph<Integer> graph,
			                         Vector<Integer> centers,
			                         HashMap<Integer,Integer> membership) {
				  options_ = options;
				  if (centers == null) {
					  System.err.println("In ComputeClustering centers == null");
					  return;
				  }
				  if (membership == null) {
					  System.err.println("In ComputeClustering membership == null");
					  return;
				  }
				  centers.clear();
				  membership.clear();
				  graph_ = graph;

				  HashSet<Integer> valid_views = new HashSet<Integer>();
				  FindValidViews(valid_views);
				  while (valid_views.size() > 0) {
				    // Find the next best canonical view.
				    double best_difference = -Double.MAX_VALUE;
				    int best_view = 0;

				    // TODO(sameeragarwal): Make this loop multi-threaded.
				    for (Integer view: valid_views) {
				      final double difference =
				          ComputeClusteringQualityDifference(view, centers);
				      if (difference > best_difference) {
				        best_difference = difference;
				        best_view = view;
				      }
				    }

				    if (best_difference == -Double.MAX_VALUE) {
				    	System.err.println("In ComputeClustering best_difference == -Double.MAX_VALUE");
				    	return;
				    }

				    // Add canonical view if quality improves, or if minimum is not
				    // yet met, otherwise break.
					    if ((best_difference <= 0) &&
					        (centers.size() >= options_.min_views)) {
					      break;
					    }

					    centers.add(best_view);
					    valid_views.remove(best_view);
					    UpdateCanonicalViewAssignments(best_view);
					  }

					  ComputeClusterMembership(centers, membership);
  
				  }

				  // Return the set of vertices of the graph which have valid vertex
			  // weights.
				  private void FindValidViews(HashSet<Integer> valid_views) {
					  final HashSet<Integer> views = graph_.vertices();
					  for (Integer view : views) {
					    if (!Double.isNaN(graph_.VertexWeight(view))) {
					      valid_views.add(view);
					    }
					  }
  
				  }
				  
				  // Computes the difference in the quality score if 'candidate' were
			  // added to the set of canonical views.
			  private double ComputeClusteringQualityDifference(int candidate,
			                                            Vector<Integer> centers) {
				// View score.
				  double difference =
				      options_.view_score_weight * graph_.VertexWeight(candidate);

				  // Compute how much the quality score changes if the candidate view
				  // was added to the list of canonical views and its nearest
				  // neighbors became members of its cluster.
				  final HashSet<Integer> neighbors = graph_.Neighbors(candidate);
				  for (Integer neighbor: neighbors) {
				    final double old_similarity = view_to_canonical_view_similarity_.getOrDefault(neighbor, 0.0); 
				    final double new_similarity = graph_.EdgeWeight(neighbor, candidate);
				    if (new_similarity > old_similarity) {
				      difference += new_similarity - old_similarity;
				    }
				  }

				  // Number of views penalty.
				  difference -= options_.size_penalty_weight;

				  // Orthogonality.
				  for (int i = 0; i < centers.size(); ++i) {
				    difference -= options_.similarity_penalty_weight *
				        graph_.EdgeWeight(centers.get(i), candidate);
				  }

				  return difference;

			  }
			  
			  // Reassign views if they're more similar to the new canonical view.
				  private void UpdateCanonicalViewAssignments(int canonical_view) {
					  final HashSet<Integer> neighbors = graph_.Neighbors(canonical_view);
					  for (Integer neighbor: neighbors) {
					    final double old_similarity = view_to_canonical_view_similarity_.getOrDefault(neighbor, 0.0); 
					    final double new_similarity =
					        graph_.EdgeWeight(neighbor, canonical_view);
					    if (new_similarity > old_similarity) {
					      view_to_canonical_view_.put(neighbor,canonical_view);
					      view_to_canonical_view_similarity_.put(neighbor,new_similarity);
					    }
					  }
  
				  }
				  
				  // Assign a cluster id to each view.
			  private void ComputeClusterMembership(Vector<Integer> centers,
			                                HashMap<Integer,Integer> membership) {
				  if (membership == null) {
					  System.err.println("membership == null in ComputeClusterMembership");
					  return;
				  }
				  membership.clear();

				  // The i^th cluster has cluster id i.
				  HashMap<Integer, Integer> center_to_cluster_id = new HashMap<Integer, Integer>();
				  for (int i = 0; i < centers.size(); ++i) {
				    center_to_cluster_id.put(centers.get(i),i);
				  }

				  final int kInvalidClusterId = -1;

				  final HashSet<Integer> views = graph_.vertices();
				  for (Integer view : views) {
				    Integer value = view_to_canonical_view_.get(view);
				    int cluster_id = kInvalidClusterId;
				    if (value != null) {
				    	if (center_to_cluster_id.get(value) == null) {
				    		System.err.println("In ComputeClusterMembership center_to_cluster_id.get(value) == null");
				    		System.err.println("value.intValue() = " + value.intValue());
				    		return;
				    	}
				    	else {
				    		cluster_id = center_to_cluster_id.get(value);
				    	}
				    }
				    if (membership.get(view) != null) {
				    	System.err.println("In ComputeClusterMembership key view is already present");
				    	System.err.println("view.intValue() = " + view.intValue());
					    	return;
					    }
					    membership.put(view, cluster_id);
					  }
  
				  }

				  
			};
			
		public void ComputeCanonicalViewsClustering(
			    CanonicalViewsClusteringOptions options,
			    WeightedGraph<Integer> graph,
			    Vector<Integer> centers,
			    HashMap<Integer,Integer> membership) {
			  long start_time = System.currentTimeMillis();
			  CanonicalViewsClustering cv = new CanonicalViewsClustering();
			  cv.ComputeClustering(options, graph, centers, membership);
			  if (2 <= MAX_LOG_LEVEL) {
			  Preferences.debug("Canonical views clustering time (secs): "
		          + (System.currentTimeMillis() - start_time)/1000.0 + "\n", Preferences.DEBUG_ALGORITHM);
		  }
		}

		// A thread safe square block sparse implementation of
		// BlockRandomAccessMatrix. Internally a TripletSparseMatrix is used
		// for doing the actual storage. This class augments this matrix with
		// an unordered_map that allows random read/write access.
		public class BlockRandomAccessSparseMatrix extends BlockRandomAccessMatrix {
		  public final long kMaxRowBlocks;
		  // row/column block sizes.
		  private final Vector<Integer> blocks_;
		  private Vector<Integer> block_positions_;
		  // A mapping from <row_block_id, col_block_id> to the position in
		  // the values array of tsm_ where the block is stored.
		  private HashMap<Long, CellInfo> layout_;
		  
		  // In order traversal of contents of the matrix. This allows us to
		  // implement a matrix-vector which is 20% faster than using the
		  // iterator in the Layout object instead.
		  private Vector<Pair<Pair<Integer, Integer>, Pair<double[], Integer> >> cell_values_;
		  // The underlying matrix object which actually stores the cells.
		  private TripletSparseMatrix tsm_;
		  // blocks is an array of block sizes. block_pairs is a set of
		  // <row_block_id, col_block_id> pairs to identify the non-zero cells
		  // of this matrix.
		  public BlockRandomAccessSparseMatrix(Vector<Integer> blocks, Set<Pair<Integer, Integer> > block_pairs) {
			  super();
			  kMaxRowBlocks = 10 * 1000 * 1000;
		      blocks_ = blocks;
		      if (blocks.size() >= kMaxRowBlocks) {
		    	  System.err.println("In public BlockRandomAccessSparseMatrix blocks.size() >= kMaxRowBlocks");
		    	  return;
		      }
		      block_positions_ = new Vector<Integer>();
		      layout_ = new HashMap<Long, CellInfo>();
		      cell_values_ = new Vector<Pair<Pair<Integer, Integer>, Pair<double[], Integer> >>();

		      // Build the row/column layout vector and count the number of scalar
		      // rows/columns.
		      int num_cols = 0;
		      block_positions_.ensureCapacity(blocks_.size());
		      for (int i = 0; i < blocks_.size(); ++i) {
		        block_positions_.add(num_cols);
		        num_cols += blocks_.get(i);
		      }

		      // Count the number of scalar non-zero entries and build the layout
		      // object for looking into the values array of the
		      // TripletSparseMatrix.
		      int num_nonzeros = 0;
		      for (Pair<Integer, Integer> pair : block_pairs) {
		        final int row_block_size = blocks_.get((int) pair.first);
		        final int col_block_size = blocks_.get((int) pair.second);
		        num_nonzeros += row_block_size * col_block_size;
		      }

		      if (1 <= MAX_LOG_LEVEL) {
		          Preferences.debug("TripletSparseMatrix Size ["+num_cols+","+num_cols+"]\n",Preferences.DEBUG_ALGORITHM);
		          Preferences.debug("num_nonzeros = " + num_nonzeros + "\n",Preferences.DEBUG_ALGORITHM);
		      }

		      tsm_ = new TripletSparseMatrix(num_cols, num_cols, num_nonzeros);
		      tsm_.set_num_nonzeros(num_nonzeros);
		      int[] rows = tsm_.mutable_rows();
		      int[] cols = tsm_.mutable_cols();
		      double[] values = tsm_.mutable_values();

		      int pos = 0;
		      for (Pair<Integer, Integer> pair : block_pairs) {
			        final int row_block_size = blocks_.get((int) pair.first);
			        final int col_block_size = blocks_.get((int) pair.second);
		            cell_values_.add(new Pair<Pair<Integer, Integer>, Pair<double[], Integer> >(new Pair<Integer,Integer>(pair.first, pair.second),
		                                         new Pair<double[],Integer>(values,pos)));
		        layout_.put(IntPairToLong(pair.first, pair.second),
		            new CellInfo(values,pos));
		        pos += row_block_size * col_block_size;
		      }

		      // Fill the sparsity pattern of the underlying matrix.
		      for (Pair<Integer, Integer> pair : block_pairs) {
		        final int row_block_id = pair.first;
		        final int col_block_id = pair.second;
		        final int row_block_size = blocks_.get(row_block_id);
		        final int col_block_size = blocks_.get(col_block_id);
		        pos = layout_.get(IntPairToLong(row_block_id, col_block_id)).values_index;
		        for (int r = 0; r < row_block_size; ++r) {
		          for (int c = 0; c < col_block_size; ++c, ++pos) {
		              rows[pos] = block_positions_.get(row_block_id) + r;
		              cols[pos] = block_positions_.get(col_block_id) + c;
		              values[pos] = 1.0;
		              if (rows[pos] >= tsm_.num_rows()) {
		            	  System.err.println("In public BlockRandomAccessSparseMatrix rows[pos] >= tsm_.num_rows()");
		            	  return;
		              }
		              if (cols[pos] >= tsm_.num_rows()) {
		            	  System.err.println("In public BlockRandomAccessSparseMatrix cols[pos] >= tsm_.num_rows()");
		            	  return;
		              }
		          }
		        }
		      }
		  }

		  // The destructor is not thread safe. It assumes that no one is
		  // modifying any cells when the matrix is being destroyed.
		  // Assume that the user does not hold any locks on any cell blocks
		  // when they are calling SetZero.
		  public void finalize() {
			  Iterator<Entry<Long, CellInfo>> it1 = layout_.entrySet().iterator(); 
			  while (it1.hasNext()) {
				  Map.Entry<Long, CellInfo> pair = (Map.Entry<Long, CellInfo>) it1.next();
				  CellInfo ci = pair.getValue();
				  ci.finalize();
				  ci = null;
			  }
		  }

		  // BlockRandomAccessMatrix Interface.
		  public  CellInfo GetCell(int row_block_id,
		                            int col_block_id,
		                            int[] row,
		                            int[] col,
		                            int[] row_stride,
		                            int[] col_stride) {
			  CellInfo ci = layout_.get(IntPairToLong(row_block_id, col_block_id));
				  if (ci == null) {
				    return null;
				  }

				  // Each cell is stored contiguously as its own little dense matrix.
				  row[0] = 0;
				  col[0] = 0;
				  row_stride[0] = blocks_.get(row_block_id);
				  col_stride[0] = blocks_.get(col_block_id);
				  return ci;
		  }

		  // This is not a thread safe method, it assumes that no cell is
		  // locked.
		  // Assume that the user does not hold any locks on any cell blocks
		  // when they are calling SetZero.
		  public void SetZero() {
			  if (tsm_.num_nonzeros() > 0) {
				  for (int i = 0; i < tsm_.num_nonzeros(); i++) {
					  tsm_.mutable_values()[i] = 0.0;
				  }
			  }
		  }

		  // Assume that the matrix is symmetric and only one half of the
		  // matrix is stored.
		  //
		  // y += S * x
		  public void SymmetricRightMultiply(double[] x, double[] y) {
			     int i;
				 for (i = 0; i < cell_values_.size(); i++) {
				    final int row = cell_values_.get(i).first.first;
				    final int row_block_size = blocks_.get(row);
				    final int row_block_pos = block_positions_.get(row);

				    final int col = cell_values_.get(i).first.second;
				    final int col_block_size = blocks_.get(col);
				    final int col_block_pos = block_positions_.get(col);

				    MatrixVectorMultiply(DYNAMIC, DYNAMIC, 1,
				        cell_values_.get(i).second.first, cell_values_.get(i).second.second, row_block_size, col_block_size,
				        x, col_block_pos,
				        y, row_block_pos);

				    // Since the matrix is symmetric, but only the upper triangular
				    // part is stored, if the block being accessed is not a diagonal
				    // block, then use the same block to do the corresponding lower
				    // triangular multiply also.
				    if (row != col) {
				      MatrixTransposeVectorMultiply(DYNAMIC, DYNAMIC, 1,
				    		  cell_values_.get(i).second.first, cell_values_.get(i).second.second, row_block_size, col_block_size,
				          x, row_block_pos,
				          y, col_block_pos);
				    }
				  }

		  }

		  // Since the matrix is square, num_rows() == num_cols().
		  public int num_rows() { return tsm_.num_rows(); }
		  public int num_cols() { return tsm_.num_cols(); }

		  // Access to the underlying matrix object.
		  public TripletSparseMatrix matrix() { return tsm_; }
		  public TripletSparseMatrix mutable_matrix() { return tsm_; }

		 public long IntPairToLong(int row, int col) {
		    return row * kMaxRowBlocks + col;
		  }

		  public void LongToIntPair(long index, int[] row, int[] col) {
		    row[0] = (int)(index / kMaxRowBlocks);
		    col[0] = (int)(index % kMaxRowBlocks);
		  }
		};
			
		// This class allows you to apply different conditioning to the residual
		// values of a wrapped cost function. An example where this is useful is
		// where you have an existing cost function that produces N values, but you
		// want the total cost to be something other than just the sum of these
		// squared values - maybe you want to apply a different scaling to some
		// values, to change their contribution to the cost.
		//
		// Usage:
		//
		//   // my_cost_function produces N residuals
		//   CostFunction* my_cost_function = ...
		//   CHECK_EQ(N, my_cost_function->num_residuals());
		//   vector<CostFunction*> conditioners;
		//
		//   // Make N 1x1 cost functions (1 parameter, 1 residual)
		//   CostFunction* f_1 = ...
		//   conditioners.push_back(f_1);
		//   ...
		//   CostFunction* f_N = ...
		//   conditioners.push_back(f_N);
		//   ConditionedCostFunction* ccf =
        //			     new ConditionedCostFunction(my_cost_function, conditioners);
		//
		// Now ccf's residual i (i=0..N-1) will be passed though the i'th conditioner.
		//
		//   ccf_residual[i] = f_i(my_cost_function_residual[i])
		//
		// and the Jacobian will be affected appropriately.
		public class ConditionedCostFunction extends CostFunction {
			private CostFunction wrapped_cost_function_;
		    private Vector<CostFunction> conditioners_;
		    private Ownership ownership_;
	
		  // Builds a cost function based on a wrapped cost function, and a
		  // per-residual conditioner. Takes ownership of all of the wrapped cost
		  // functions, or not, depending on the ownership parameter. Conditioners
		  // may be NULL, in which case the corresponding residual is not modified.
		    public ConditionedCostFunction(CostFunction wrapped_cost_function,
		                          Vector<CostFunction> conditioners,
		                          Ownership ownership) {
		    	super();
		    	wrapped_cost_function_ = wrapped_cost_function;
		        conditioners_ = conditioners;
		        ownership_ = ownership;
		        // Set up our dimensions.
		        set_num_residuals(wrapped_cost_function_.num_residuals());
		        parameter_block_sizes_ =
		            wrapped_cost_function_.parameter_block_sizes();

		        // Sanity-check the conditioners' dimensions.
		        if (wrapped_cost_function_.num_residuals() != conditioners_.size()) {
		        	System.err.println("In public ConditionedCostFunction wrapped_cost_function_.num_residuals() != conditioners_.size()");
		        	return;
		        }
		        for (int i = 0; i < wrapped_cost_function_.num_residuals(); i++) {
		          if (conditioners.get(i) != null) {
		            if (1 != conditioners.get(i).num_residuals()) {
		            	System.err.println("In public ConditionedCostFunction 1 != conditioners.get("+i+").num_residuals()");
		            	return;
		            }
		            if (1 != conditioners.get(i).parameter_block_sizes().size()) {
		            	System.err.println("In public ConditionedCostFunction 1 != conditioners.get("+i+").parameter_block_sizes().size()");
		            	return;
		            }
		            if (1 != conditioners.get(i).parameter_block_sizes().get(0)) {
		            	System.err.println("In public ConditionedCostFunction 1 != conditioners.get("+i+").parameter_block_sizes().get(0)");
		            	return;
		            }
		          }
		        }

		    }
		  
		    
		    public void finalize() {
		    	if (ownership_ == Ownership.TAKE_OWNERSHIP) {
		    	    for (int i = 0; i < conditioners_.size(); i++) {
		    	    	CostFunction cf = conditioners_.get(i);
		    	    	cf = null;
		    	    }
		    	}
		    }

		  public boolean Evaluate(Vector<double[]> parameters,
		                        double[] residuals,
		                        double[][] jacobians) {
			  boolean success = wrapped_cost_function_.Evaluate(parameters, residuals,
                      jacobians);
			if (!success) {
			return false;
			}
			
			for (int r = 0; r < wrapped_cost_function_.num_residuals(); r++) {
			// On output, we want to have
			// residuals[r] = conditioners[r](wrapped_residuals[r])
			// For parameter block i, column c,
			// jacobians[i][r*parameter_block_size_[i] + c] =
			//   = d residual[r] / d parameters[i][c]
			//   = conditioners[r]'(wrapped_residuals[r]) *
			//       d wrapped_residuals[r] / d parameters[i][c]
			if (conditioners_.get(r) != null) {
			double[][] conditioner_derivative_pointer2;
			if (jacobians == null) {
			conditioner_derivative_pointer2 = null;
			}
			else {
				conditioner_derivative_pointer2 = new double[1][1];
			}
			
			double res[] = new double[] {residuals[r]};
			Vector<double[]>parameters2 = new Vector<>();
			parameters2.add(res);
			success = conditioners_.get(r).Evaluate(parameters2,
			                   res,
			                   conditioner_derivative_pointer2);
			residuals[r] = res[0];
			if (!success) {
			return false;
			}
			
			if (jacobians != null) {
			for (int i = 0;
			i < wrapped_cost_function_.parameter_block_sizes().size();
			i++) {
			if (jacobians[i] != null) {
			int parameter_block_size =
			wrapped_cost_function_.parameter_block_sizes().get(i);
			for (int j = 0; j < parameter_block_size; j++) {
				jacobians[i][j + r * parameter_block_size] *= conditioner_derivative_pointer2[0][0];
			}
			}
			}
			}
			}
			}
			return true;

		  }

		 
		};
			
		// Extract the block sparsity pattern of the scalar compressed columns
		// matrix and return it in compressed column form. The compressed
		// column form is stored in two vectors block_rows, and block_cols,
		// which correspond to the row and column arrays in a compressed
		// column sparse matrix.
		//
		// If c_ij is the block in the matrix A corresponding to row block i
		// and column block j, then it is expected that A contains at least
		// one non-zero entry corresponding to the top left entry of c_ij,
		// as that entry is used to detect the presence of a non-zero c_ij.
		public void CompressedColumnScalarMatrixToBlockMatrix(
    	     int[] scalar_rows,
    	     int[] scalar_cols,
    	     Vector<Integer> row_blocks,
    	     Vector<Integer> col_blocks,
    	     Vector<Integer> block_rows,
    	     Vector<Integer> block_cols) {
    	     if (block_rows == null) {
    	    	 System.err.println("In public void CompressedColumnScalarMatrixToBlockMatrix block_rows == null");
    	    	 return;
    	     }
    	     if (block_cols == null) {
    	    	 System.err.println("In public void CompressedColumnScalarMatrixToBlockMatrix block_cols == null");
    	    	 return;
    	     }
    	     block_rows.clear();
    	     block_cols.clear();
    	     final int num_row_blocks = row_blocks.size();
    	     final int num_col_blocks = col_blocks.size();

    	  Vector<Integer> row_block_starts = new Vector<Integer>(num_row_blocks);
    	  for (int i = 0, cursor = 0; i < num_row_blocks; ++i) {
    	    row_block_starts.add(cursor);
    	    cursor += row_blocks.get(i);
    	  }

    	  // This loop extracts the block sparsity of the scalar sparse matrix
    	  // It does so by iterating over the columns, but only considering
    	  // the columns corresponding to the first element of each column
    	  // block. Within each column, the inner loop iterates over the rows,
    	  // and detects the presence of a row block by checking for the
    	  // presence of a non-zero entry corresponding to its first element.
    	  block_cols.add(0);
    	  int c = 0;
    	  for (int col_block = 0; col_block < num_col_blocks; ++col_block) {
    	    int column_size = 0;
    	    for (int idx = scalar_cols[c]; idx < scalar_cols[c + 1]; ++idx) {
    	    int it = 0;
    	    for (; it < row_block_starts.size(); it++) {
    	    	if (row_block_starts.get(it) >= scalar_rows[idx]) {
    	    		break;
    	    	}
    	    }
    	      
    	      // Since we are using lower_bound, it will return the row id
    	      // where the row block starts. For everything but the first row
    	      // of the block, where these values will be the same, we can
    	      // skip, as we only need the first row to detect the presence of
    	      // the block.
    	      //
    	      // For rows all but the first row in the last row block,
    	      // lower_bound will return row_block_starts.end(), but those can
    	      // be skipped like the rows in other row blocks too.
    	      if (it == row_block_starts.size() || row_block_starts.get(it).intValue() != scalar_rows[idx]) {
    	        continue;
    	      }

    	      block_rows.add(it);
    	      ++column_size;
    	    }
    	    block_cols.add(block_cols.lastElement() + column_size);
    	    c += col_blocks.get(col_block);
    	  }
    	}

	// Given a set of blocks and a permutation of these blocks, compute
	// the corresponding "scalar" ordering, where the scalar ordering of
	// size sum(blocks).
	public void BlockOrderingToScalarOrdering(Vector<Integer> blocks,
            Vector<Integer> block_ordering,
            Vector<Integer> scalar_ordering) {
		if (blocks.size() != block_ordering.size()) {
			System.err.println("In public void BlockOrderingToScalarOrdering blocks.size() != block_ordering.size()");
			return;
		}
		final int num_blocks = blocks.size();
		
		// block_starts = [0, block1, block1 + block2 ..]
		Vector<Integer> block_starts = new Vector<Integer>(num_blocks);
		for (int i = 0, cursor = 0; i < num_blocks ; ++i) {
		block_starts.add(cursor);
		cursor += blocks.get(i);
		}
		
		while (scalar_ordering.size() > (block_starts.lastElement() + blocks.lastElement())) {
			scalar_ordering.removeElementAt(scalar_ordering.size()-1);
		}
		while (scalar_ordering.size() < (block_starts.lastElement() + blocks.lastElement())) {
			scalar_ordering.add(0);
		}
		int cursor = 0;
		for (int i = 0; i < num_blocks; ++i) {
		final int block_id = block_ordering.get(i);
		final int block_size = blocks.get(block_id);
		int block_position = block_starts.get(block_id);
		for (int j = 0; j < block_size; ++j) {
		scalar_ordering.set(cursor++,block_position++);
		}
	}
        
	}
	
	// Solve the linear system
	//
	//   R * solution = rhs
	//
	// Where R is an upper triangular compressed column sparse matrix
	public void SolveUpperTriangularInPlace(int num_cols,
	                                 int[] rows,
	                                 int[] cols,
	                                 double[] values,
	                                 double[] rhs_and_solution) {
	  for (int c = num_cols - 1; c >= 0; --c) {
	    rhs_and_solution[c] /= values[cols[c + 1] - 1];
	    for (int idx = cols[c]; idx < cols[c + 1] - 1; ++idx) {
	      final int r = rows[idx];
	      final double v = values[idx];
	      rhs_and_solution[r] -= v * rhs_and_solution[c];
	    }
	  }
	}
	
	// Solve the linear system
	//
	//   R' * solution = rhs
	//
	// Where R is an upper triangular compressed column sparse matrix.
	public void SolveUpperTriangularTransposeInPlace(int num_cols,
	                                          int[] rows,
	                                          int[] cols,
	                                          double[] values,
	                                          double[] rhs_and_solution) {
	  for (int c = 0; c < num_cols; ++c) {
	    for (int idx = cols[c]; idx < cols[c + 1] - 1; ++idx) {
	      final int r = rows[idx];
	      final double v = values[idx];
	      rhs_and_solution[c] -= v * rhs_and_solution[r];
	    }
	    rhs_and_solution[c] =  rhs_and_solution[c] / values[cols[c + 1] - 1];
	  }
	}
	
	// Given a upper triangular matrix R in compressed column form, solve
	// the linear system,
	//
	//  R'R x = b
	//
	// Where b is all zeros except for rhs_nonzero_index, where it is
	// equal to one.
	//
	// The function exploits this knowledge to reduce the number of
	// floating point operations.
	public void SolveRTRWithSparseRHS(int num_cols,
	                           int[] rows,
	                           int[] cols,
	                           double[] values,
	                           int rhs_nonzero_index,
	                           double[] solution) {
	  int i;
	  for (i = 0; i < num_cols; i++) {
		  solution[i] = 0.0;
	  }
	  solution[rhs_nonzero_index] = 1.0 / values[cols[rhs_nonzero_index + 1] - 1];

	  for (int c = rhs_nonzero_index + 1; c < num_cols; ++c) {
	    for (int idx = cols[c]; idx < cols[c + 1] - 1; ++idx) {
	      final int r = rows[idx];
	      if (r < rhs_nonzero_index) continue;
	      final double v = values[idx];
	      solution[c] -= v * solution[r];
	    }
	    solution[c] =  solution[c] / values[cols[c + 1] - 1];
	  }

	  SolveUpperTriangularInPlace(num_cols, rows, cols, values, solution);
	}
	
	public class Triplet<T, U, V>
	{
	    public T first;
	    public U second;
	    public V third;

	    public Triplet(T t, U u, V v)
	    {
	        first = t;
	        second = u;
	        third = v;
	    }
	    
	    public T getFirst()
	    {
	        return first; 
	    }

	    public U getSecond()
	    {
	        return second; 
	    }
	    
	    public V getThird() {
	    	return third;
	    }

	    
	    public boolean equals(Object obj) {
	      if (obj == null) return false;
	      if ((obj.getClass() != this.getClass())) { //|| (obj.hashCode() != this.hashCode())) {
	        return false;
	      }
	      
	      return (this.getFirst().equals(((Triplet) obj).getFirst()) && this.getSecond().equals(((Triplet) obj).getSecond())
	    		  && this.getThird().equals(((Triplet) obj).getThird()));
	    }
	    
	    /**
	     * Define a hash code based on the first and seconds and third's hash code
	     */
	    public int hashCode() {
	      return first.hashCode() ^ second.hashCode() ^ third.hashCode();
	    }
	    
	    public String toString() {
	      return "Triplet(" + first + ", " + second +  ", " + third+ ")";
	    }
	  
	}
    	
	public enum StorageType {
	    UNSYMMETRIC,
	    // Matrix is assumed to be symmetric but only the lower triangular
	    // part of the matrix is stored.
	    LOWER_TRIANGULAR,
	    // Matrix is assumed to be symmetric but only the upper triangular
	    // part of the matrix is stored.
	    UPPER_TRIANGULAR
	  };
	  
	  // Options struct to control the generation of random block sparse
	  // matrices in compressed row sparse format.
	  //
	  // The random matrix generation proceeds as follows.
	  //
	  // First the row and column block structure is determined by
	  // generating random row and column block sizes that lie within the
	  // given bounds.
	  //
	  // Then we walk the block structure of the resulting matrix, and with
	  // probability block_density detemine whether they are structurally
	  // zero or not. If the answer is no, then we generate entries for the
	  // block which are distributed normally.
	  public class CompressedRowSparseMatrixRandomMatrixOptions {
		    public int num_row_blocks;
		    public int min_row_block_size;
		    public int max_row_block_size;
		    public int num_col_blocks;
		    public int min_col_block_size;
		    public int max_col_block_size;

		    // 0 < block_density <= 1 is the probability of a block being
		    // present in the matrix. A given random matrix will not have
		    // precisely this density.
		    public double block_density;
	    public CompressedRowSparseMatrixRandomMatrixOptions() {
	          num_row_blocks = 0;
	          min_row_block_size = 0;
	          max_row_block_size = 0;
	          num_col_blocks = 0;
	          min_col_block_size = 0;
	          max_col_block_size = 0;
	          block_density = 0.0;
	    }

	    
	  };
	  
	  private class indexRowColItem {
		    private int index;
			private int row;
			private int col;
			
			public indexRowColItem(int index, int row, int col) {
				this.index = index;
				this.row = row;
				this.col = col;
			}
			
			public int getIndex() {
				return index;
			}
			
			public int getRow() {
				return row;
			}
			
			public int getCol() {
				return col;
			}
			
			
		}
	  
	// Helper functor used by the constructor for reordering the contents
	// of a TripletSparseMatrix. This comparator assumes thay there are no
	// duplicates in the pair of arrays rows and cols, i.e., there is no
	// indices i and j (not equal to each other) s.t.
	//
	//  rows[i] == rows[j] && cols[i] == cols[j]
	//
	// If this is the case, this functor will not be a StrictWeakOrdering.
	public class RowColLessThanComparator implements Comparator<indexRowColItem> {
		
	 

	  public int compare(indexRowColItem x, indexRowColItem y) {
		  if (x.getRow() == y.getRow()) {
		      return (x.getCol() - y.getCol());
		    }
		    return (x.getRow() - y.getRow());
	  }

	

	

	 
	};

	
	
	public void AddRandomBlock(int num_rows,
            int num_cols,
            int row_block_begin,
            int col_block_begin,
            Vector<Integer> rows,
            Vector<Integer> cols,
            Vector<Double> values) {
		for (int r = 0; r < num_rows; ++r) {
			for (int c = 0; c < num_cols; ++c) {
				rows.add(row_block_begin + r);
				cols.add(col_block_begin + c);
				values.add(RandNormal());
			}
		}
	}
	
	public CompressedRowSparseMatrix  FromTripletSparseMatrix(TripletSparseMatrix input) {
		  return FromTripletSparseMatrix(input, false);
	}
	
	public CompressedRowSparseMatrix  FromTripletSparseMatrixTransposed(TripletSparseMatrix input) {
		  return FromTripletSparseMatrix(input, true);
	}

	public CompressedRowSparseMatrix FromTripletSparseMatrix(
		    TripletSparseMatrix input, boolean transpose) {
		  int i;
		  int num_rows = input.num_rows();
		  int num_cols = input.num_cols();
		  int[] rows = input.rows();
		  int[] cols = input.cols();
		  final double[] values = input.values();

		  if (transpose) {
			int tmp;
			int tmp_array[];
			tmp = num_rows;
			num_rows = num_cols;
			num_cols = tmp;
		    tmp_array = rows;
		    rows = cols;
		    cols = tmp_array;
		  }

		  
		  

		  // Sort index such that the entries of m are ordered by row and ties
		  // are broken by column.
		  ArrayList <indexRowColItem> ircList = new ArrayList<indexRowColItem>();
		  for (i = 0; i < input.num_nonzeros(); ++i) {
			  ircList.add(new indexRowColItem(i,rows[i],cols[i]));
		  }
		  Collections.sort(ircList, new RowColLessThanComparator());
		  // index is the list of indices into the TripletSparseMatrix input.
		  int index[] = new int[input.num_nonzeros()];
		  for (i = 0; i < input.num_nonzeros(); i++) {
			  index[i] = ircList.get(i).getIndex();
		  }
		  
		  if (1 <= MAX_LOG_LEVEL) {
		        Preferences.debug("# of rows: " + num_rows + "\n", Preferences.DEBUG_ALGORITHM);
		        Preferences.debug("# of columns: " + num_cols + "\n", Preferences.DEBUG_ALGORITHM);
		        Preferences.debug("max_num_nonzeros: " + cols.length + "\n", Preferences.DEBUG_ALGORITHM);
		    }

		  
		  CompressedRowSparseMatrix output =
		      new CompressedRowSparseMatrix(num_rows, num_cols, input.num_nonzeros());

		  // Copy the contents of the cols and values array in the order given
		  // by index and count the number of entries in each row.
		  int[] output_rows = output.mutable_rows();
		  int[] output_cols = output.mutable_cols();
		  double[] output_values = output.mutable_values();

		  output_rows[0] = 0;
		  for (i = 0; i < index.length; ++i) {
		    final int idx = index[i];
		    ++output_rows[rows[idx] + 1];
		    output_cols[i] = cols[idx];
		    output_values[i] = values[idx];
		  }

		  // Find the cumulative sum of the row counts.
		  for (i = 1; i < num_rows + 1; ++i) {
		    output_rows[i] += output_rows[i - 1];
		  }

		  if (output.num_nonzeros() != input.num_nonzeros()) {
			  System.err.println("In public CompressedRowSparseMatrix FromTripletSparseMatrix output.num_nonzeros() != input.num_nonzeros()");
			  return null;
		  }
		  return output;
		}
	
	public CompressedRowSparseMatrix CreateBlockDiagonalMatrix(
		    double[] diagonal, Vector<Integer> blocks) {
		  int i, r;
		  int num_rows = 0;
		  int num_nonzeros = 0;
		  for (i = 0; i < blocks.size(); ++i) {
		    num_rows += blocks.get(i);
		    num_nonzeros += blocks.get(i) * blocks.get(i);
		  }

		  CompressedRowSparseMatrix matrix =
		      new CompressedRowSparseMatrix(num_rows, num_rows, num_nonzeros);

		  int[] rows = matrix.mutable_rows();
		  int[] cols = matrix.mutable_cols();
		  double[] values = matrix.mutable_values();
		  for (i = 0; i < num_nonzeros; i++) {
			  values[i] = 0.0;
		  }

		  int idx_cursor = 0;
		  int col_cursor = 0;
		  int rows_index = 0;
		  int cols_index = 0;
		  for (i = 0; i < blocks.size(); ++i) {
		    final int block_size = blocks.get(i);
		    for (r = 0; r < block_size; ++r) {
		      rows[rows_index++] = idx_cursor;
		      values[idx_cursor + r] = diagonal[col_cursor + r];
		      for (int c = 0; c < block_size; ++c, ++idx_cursor) {
		        cols[cols_index++] = col_cursor + c;
		      }
		    }
		    col_cursor += block_size;
		  }
		  rows[rows_index] = idx_cursor;

		  matrix.set_row_blocks(blocks);
		  matrix.set_col_blocks(blocks);

		  if (idx_cursor != num_nonzeros) {
			  System.err.println("In CompressedRowSparseMatrix CreateBlockDiagonalMatrix idx_cursor != num_nonzeros");
			  return null;
		  }
		  if (col_cursor != num_rows) {
			  System.err.println("In CompressedRowSparseMatrix CreateBlockDiagonalMatrix col_cursor != num_rows");
			  return null;
		  }
		  return matrix;
		}
	
	public CompressedRowSparseMatrix CreateRandomMatrix(CompressedRowSparseMatrixRandomMatrixOptions options) {
		  int i;
		  RandomNumberGen randomGen = new RandomNumberGen();
		  
		  if (options.min_row_block_size <= 0) {
			  System.err.println("In CompressedRowSparseMatrix CreateRandomMatrix options.min_row_block_size <= 0");
			  return null;
		  }
		  if (options.max_row_block_size <= 0) {
			  System.err.println("In CompressedRowSparseMatrix CreateRandomMatrix options.max_row_block_size <= 0");
			  return null;
		  }
		  if (options.min_row_block_size > options.max_row_block_size) {
			  System.err.println("In CompressedRowSparseMatrix CreateRandomMatrix options.min_row_block_size > options.max_row_block_size");
			  return null;
		  }
		  if (options.num_col_blocks <= 0) {
			  System.err.println("In CompressedRowSparseMatrix CreateRandomMatrix options.num_col_blocks <= 0");
			  return null;
		  }
		  if (options.min_col_block_size <= 0) {
			  System.err.println("In CompressedRowSparseMatrix CreateRandomMatrix options.min_col_block_size <= 0");
			  return null;
		  }
		  if (options.max_col_block_size <= 0) {
			  System.err.println("In CompressedRowSparseMatrix CreateRandomMatrix options.max_col_block_size <= 0");
			  return null; 
		  }
		  if (options.min_col_block_size > options.max_col_block_size) {
			  System.err.println("In CompressedRowSparseMatrix CreateRandomMatrix options.min_col_block_size > options.max_col_block_size");
			  return null;
		  }
		  if (options.block_density <= 0.0) {
			  System.err.println("In CompressedRowSparseMatrix CreateRandomMatrix options.block_density <= 0.0");
			  return null;
		  }
		  if (options.block_density > 1.0) {
			  System.err.println("In CompressedRowSparseMatrix CreateRandomMatrix options.block_density > 1.0");
			  return null;
		  }

		  Vector<Integer> row_blocks = new Vector<Integer>();
		  Vector<Integer> col_blocks = new Vector<Integer>();

		  // Generate the row block structure.
		  for (i = 0; i < options.num_row_blocks; ++i) {
		    // Generate a random integer in [min_row_block_size, max_row_block_size]
		    final int delta_block_size =
		        randomGen.genUniformRandomNum(0,options.max_row_block_size - options.min_row_block_size);
		    row_blocks.add(options.min_row_block_size + delta_block_size);
		  }

		  // Generate the col block structure.
		  for (i = 0; i < options.num_col_blocks; ++i) {
		    // Generate a random integer in [min_col_block_size, max_col_block_size]
		    final int delta_block_size =
		        randomGen.genUniformRandomNum(0,options.max_col_block_size - options.min_col_block_size);
		    col_blocks.add(options.min_col_block_size + delta_block_size);
		  }

		  Vector<Integer> tsm_rows = new Vector<Integer>();
		  Vector<Integer> tsm_cols = new Vector<Integer>();
		  Vector<Double> tsm_values = new Vector<Double>();

		  // For ease of construction, we are going to generate the
		  // CompressedRowSparseMatrix by generating it as a
		  // TripletSparseMatrix and then converting it to a
		  // CompressedRowSparseMatrix.

		  // It is possible that the random matrix is empty which is likely
		  // not what the user wants, so do the matrix generation till we have
		  // at least one non-zero entry.
		  while (tsm_values.isEmpty()) {
		    tsm_rows.clear();
		    tsm_cols.clear();
		    tsm_values.clear();

		    int row_block_begin = 0;
		    for (int r = 0; r < options.num_row_blocks; ++r) {
		      int col_block_begin = 0;
		      for (int c = 0; c < options.num_col_blocks; ++c) {
		        // Randomly determine if this block is present or not.
		        if (RandDouble() <= options.block_density) {
		          AddRandomBlock(row_blocks.get(r),
		                         col_blocks.get(c),
		                         row_block_begin,
		                         col_block_begin,
		                         tsm_rows,
		                         tsm_cols,
		                         tsm_values);
		        }
		        col_block_begin += col_blocks.get(c);
		      }
		      row_block_begin += row_blocks.get(r);
		    }
		  }
		  
		  int num_rows = 0;
		  for (i = 0; i < row_blocks.size(); i++) {
			  num_rows += row_blocks.get(i);
		  }
		  int num_cols = 0;
		  for (i = 0; i < col_blocks.size(); i++) {
			  num_cols += col_blocks.get(i);
		  }
		  final boolean kDoNotTranspose = false;
		  CompressedRowSparseMatrix matrix = FromTripletSparseMatrix(
		          new TripletSparseMatrix(
		              num_rows, num_cols, tsm_rows, tsm_cols, tsm_values),
		          kDoNotTranspose);
		  matrix.set_row_blocks(row_blocks);
		  matrix.set_col_blocks(col_blocks);
		  matrix.set_storage_type(StorageType.UNSYMMETRIC);
		  return matrix;
		}

	  
	 
	  
	  public class DynamicCompressedRowSparseMatrix extends CompressedRowSparseMatrix {
		  private Vector<Vector<Integer> > dynamic_cols_;
		  private Vector<Vector<Double> > dynamic_values_; 
		  
		  public DynamicCompressedRowSparseMatrix(int num_rows, int num_cols, int initial_max_num_nonzeros) {
				  super(num_rows, num_cols, initial_max_num_nonzeros);
				  dynamic_cols_ = new Vector<Vector<Integer>>(num_rows);
				  dynamic_values_ = new Vector<Vector<Double>>(num_rows);
				  for (int i = 0; i < num_rows; i++) {
					  dynamic_cols_.add(new Vector<Integer>());
					  dynamic_values_.add(new Vector<Double>());
				  }
		 }
		  
		 public void InsertEntry(int row, int col, double value) {
			if (row < 0) {
				System.err.println("In DynamicCompressedRowSparseMatrix InsertEntry row < 0");
				return;
			}
			if (row >= num_rows()) {
				System.err.println("In DynamicCompressedRowSparseMatrix InsertEntry row >= num_rows()");
				return;
			}
			if (col < 0) {
				System.err.println("In DynamicCompressedRowSparseMatrix InsertEntry col < 0");
				return;
			}
			if (col >= num_cols()) {
				System.err.println("In DynamicCompressedRowSparseMatrix InsertEntry col >= num_cols()");
				return;
			}
			dynamic_cols_.get(row).add(col);
			dynamic_values_.get(row).add(value);
		 }

		 public void ClearRows(int row_start, int num_rows) {
			for (int r = 0; r < num_rows; ++r) {
				final int i = row_start + r;
				if (i < 0) {
					System.err.println("In DynamicCompressedRowSparseMatrix ClearRows i < 0");
					return;
				}
				if (i >= this.num_rows()) {
					System.err.println("In DynamicCompressedRowSparseMatrx ClearRows i >= this.num_rows()");
					return;
				}
				dynamic_cols_.get(i).clear();
				dynamic_values_.get(i).clear();
			}
         }
		 
		 public void Finalize(int num_additional_elements) {
			  // `num_additional_elements` is provided as an argument so that additional
			  // storage can be reserved when it is known by the finalizer.
			  if (num_additional_elements < 0) {
				  System.err.println("In DynamicCompressedRowSparseMatrix Finalize num_additional_elements < 0");
				  return;
			  }

			  // Count the number of non-zeros and resize `cols_` and `values_`.
			  int num_jacobian_nonzeros = 0;
			  for (int i = 0; i < dynamic_cols_.size(); ++i) {
			    num_jacobian_nonzeros += dynamic_cols_.get(i).size();
			  }

			  SetMaxNumNonZeros(num_jacobian_nonzeros + num_additional_elements);

			  // Flatten `dynamic_cols_` into `cols_` and `dynamic_values_`
			  // into `values_`.
			  int index_into_values_and_cols = 0;
			  for (int i = 0; i < num_rows(); ++i) {
			    mutable_rows()[i] = index_into_values_and_cols;
			    final int num_nonzero_columns = dynamic_cols_.get(i).size();
			    if (num_nonzero_columns > 0) {
			      for (int j = 0; j < dynamic_cols_.get(i).size(); j++) {
			    	  mutable_cols()[index_into_values_and_cols + j] = dynamic_cols_.get(i).get(j);
			      }
			      for (int j = 0; j < dynamic_values_.get(i).size(); j++) {
			    	  mutable_values()[index_into_values_and_cols + j] = dynamic_values_.get(i).get(j);
			      }
			      index_into_values_and_cols += dynamic_cols_.get(i).size();
			    }
			  }
			  mutable_rows()[num_rows()] = index_into_values_and_cols;

			  if (index_into_values_and_cols != num_jacobian_nonzeros) {
			    System.err.println("Ceres bug: final index into values_ and cols_ should be equal to ");
			     System.err.println("the number of jacobian nonzeros. Please contact the developers!");
			  }
			  
		 }


	  }
	  
	  class CovarianceOptions {
		// Sparse linear algebra library to use when a sparse matrix
		    // factorization is being used to compute the covariance matrix.
		    //
		    // Currently this only applies to SPARSE_QR.
		    public SparseLinearAlgebraLibraryType sparse_linear_algebra_library_type;

		    // Ceres supports two different algorithms for covariance
		    // estimation, which represent different tradeoffs in speed,
		    // accuracy and reliability.
		    //
		    // 1. DENSE_SVD uses Eigen's JacobiSVD to perform the
		    //    computations. It computes the singular value decomposition
		    //
		    //      U * S * V' = J
		    //
		    //    and then uses it to compute the pseudo inverse of J'J as
		    //
		    //      pseudoinverse[J'J]^ = V * pseudoinverse[S] * V'
		    //
		    //    It is an accurate but slow method and should only be used
		    //    for small to moderate sized problems. It can handle
		    //    full-rank as well as rank deficient Jacobians.
		    //
		    // 2. SPARSE_QR uses the sparse QR factorization algorithm
		    //    to compute the decomposition
		    //
		    //      Q * R = J
		    //
		    //    [J'J]^-1 = [R*R']^-1
		    //
		    // SPARSE_QR is not capable of computing the covariance if the
		    // Jacobian is rank deficient. Depending on the value of
		    // Covariance::Options::sparse_linear_algebra_library_type, either
		    // Eigen's Sparse QR factorization algorithm will be used or
		    // SuiteSparse's high performance SuiteSparseQR algorithm will be
		    // used.
		    public CovarianceAlgorithmType algorithm_type;

		    // If the Jacobian matrix is near singular, then inverting J'J
		    // will result in unreliable results, e.g, if
		    //
		    //   J = [1.0 1.0         ]
		    //       [1.0 1.0000001   ]
		    //
		    // which is essentially a rank deficient matrix, we have
		    //
		    //   inv(J'J) = [ 2.0471e+14  -2.0471e+14]
		    //              [-2.0471e+14   2.0471e+14]
		    //
		    // This is not a useful result. Therefore, by default
		    // Covariance::Compute will return false if a rank deficient
		    // Jacobian is encountered. How rank deficiency is detected
		    // depends on the algorithm being used.
		    //
		    // 1. DENSE_SVD
		    //
		    //      min_sigma / max_sigma < sqrt(min_reciprocal_condition_number)
		    //
		    //    where min_sigma and max_sigma are the minimum and maxiumum
		    //    singular values of J respectively.
		    //
		    // 2. SPARSE_QR
		    //
		    //      rank(J) < num_col(J)
		    //
		    //   Here rank(J) is the estimate of the rank of J returned by the
		    //   sparse QR factorization algorithm. It is a fairly reliable
		    //   indication of rank deficiency.
		    //
		    public double min_reciprocal_condition_number;

		    // When using DENSE_SVD, the user has more control in dealing with
		    // singular and near singular covariance matrices.
		    //
		    // As mentioned above, when the covariance matrix is near
		    // singular, instead of computing the inverse of J'J, the
		    // Moore-Penrose pseudoinverse of J'J should be computed.
		    //
		    // If J'J has the eigen decomposition (lambda_i, e_i), where
		    // lambda_i is the i^th eigenvalue and e_i is the corresponding
		    // eigenvector, then the inverse of J'J is
		    //
		    //   inverse[J'J] = sum_i e_i e_i' / lambda_i
		    //
		    // and computing the pseudo inverse involves dropping terms from
		    // this sum that correspond to small eigenvalues.
		    //
		    // How terms are dropped is controlled by
		    // min_reciprocal_condition_number and null_space_rank.
		    //
		    // If null_space_rank is non-negative, then the smallest
		    // null_space_rank eigenvalue/eigenvectors are dropped
		    // irrespective of the magnitude of lambda_i. If the ratio of the
		    // smallest non-zero eigenvalue to the largest eigenvalue in the
		    // truncated matrix is still below
		    // min_reciprocal_condition_number, then the Covariance::Compute()
		    // will fail and return false.
		    //
		    // Setting null_space_rank = -1 drops all terms for which
		    //
		    //   lambda_i / lambda_max < min_reciprocal_condition_number.
		    //
		    // This option has no effect on the SUITE_SPARSE_QR and
		    // EIGEN_SPARSE_QR algorithms.
		    public int null_space_rank;

		    public int num_threads;

		    // Even though the residual blocks in the problem may contain loss
		    // functions, setting apply_loss_function to false will turn off
		    // the application of the loss function to the output of the cost
		    // function and in turn its effect on the covariance.
		    //
		    // TODO(sameergaarwal): Expand this based on Jim's experiments.
		    public boolean apply_loss_function;
		    
		    public CovarianceOptions() {
		    	//algorithm_type = CovarianceAlgorithmType.SPARSE_QR;
		    	algorithm_type = CovarianceAlgorithmType.DENSE_SVD;

		        // Eigen's QR factorization is always available.
		        sparse_linear_algebra_library_type = SparseLinearAlgebraLibraryType.EIGEN_SPARSE;
		  //#if !defined(CERES_NO_SUITESPARSE)
		        //sparse_linear_algebra_library_type = SUITE_SPARSE;
		  //#endif

		        min_reciprocal_condition_number = 1e-14;
		        null_space_rank = 0;
		        num_threads = 1;
		        apply_loss_function = true;
		    }
	  }
	  
	  class CovarianceImpl {
		  private ProblemImpl problem_;
		  private CovarianceOptions options_;
		  private EvaluateOptions evaluate_options_;
		  private boolean is_computed_;
		  private boolean is_valid_;
		  private HashMap<double[], Integer> parameter_block_to_row_index_;
		  private HashSet<double[]> constant_parameter_blocks_;
		  private CompressedRowSparseMatrix covariance_matrix_;
		  
		  public CovarianceImpl(CovarianceOptions options) {
		      options_ = options;
		      is_computed_ = false;
		      is_valid_ = false;
		      if (options_.num_threads > 1) {
			      if (WARNING <= MAX_LOG_LEVEL) {
			        Preferences.debug("Neither OpenMP nor TBB support is compiled into this binary; \n" +
			        "only options.num_threads = 1 is supported. Switching \n" +
			        "to single threaded mode.\n", Preferences.DEBUG_ALGORITHM);
			        options_.num_threads = 1;
			      }
		     }

		  evaluate_options_ = new EvaluateOptions();
		  evaluate_options_.num_threads = options_.num_threads;
		  evaluate_options_.apply_loss_function = options_.apply_loss_function;
		  constant_parameter_blocks_ = new HashSet<double[]>();
		  parameter_block_to_row_index_ = new HashMap<double[], Integer>();
		}
		  
		  public CompressedRowSparseMatrix covariance_matrix() {
			    return covariance_matrix_;
		  }
		  
		  public boolean CheckForDuplicates(Vector<double[]> blocks) {
		      // Pair can have first and second as the same array or as different arrays
		      // Pair can have arrays of the same or different lengths
			int i;
			boolean status = true;
			ArrayList<indexArrayItem> ia = new ArrayList<indexArrayItem>();
			for (i = 0; i < blocks.size(); i++) {
				ia.add(new indexArrayItem(i,blocks.get(i)));
			}
			indexArrayComparator ic = new indexArrayComparator();
			Collections.sort(ia, ic);
			for (i = 0; i < blocks.size()-1; i++) {
				if (ic.compare(ia.get(i), ia.get(i+1)) == 0) {
					status = false;
					System.err.println("Covariance::Compute called with duplicate blocks at indices " + ia.get(i).getIndex() + " and " + ia.get(i+1).getIndex());
				}
			}
			return status;
			
		}
		  
		public boolean CheckForPairDuplicates(Vector<Pair<double[], double[]>> blocks) {
		      // Pair can have first and second as the same array or as different arrays
		      // Pair can have arrays of the same or different lengths
			int i;
			boolean status = true;
			ArrayList<indexArrayArrayItem> iaa = new ArrayList<indexArrayArrayItem>();
			for (i = 0; i < blocks.size(); i++) {
				iaa.add(new indexArrayArrayItem(i,blocks.get(i).getFirst(),blocks.get(i).getSecond()));
			}
			indexArrayArrayComparator ic = new indexArrayArrayComparator();
			Collections.sort(iaa, ic);
			for (i = 0; i < blocks.size()-1; i++) {
				if (ic.compare(iaa.get(i), iaa.get(i+1)) == 0) {
					status = false;
					System.err.println("Covariance::pairCompute called with duplicate blocks at indices " + iaa.get(i).getIndex() + " and " + iaa.get(i+1).getIndex());
				}
			}
			return status;
			
		}
		
		public boolean Compute(Vector<double[]> parameter_blocks,
                ProblemImpl problem) {
		    boolean status = CheckForDuplicates(parameter_blocks);
		    if (!status) {
		    	return false;
		    }
			Vector<Pair<double[], double[]>> covariance_blocks = new Vector<Pair<double[], double[]>>();
			for (int i = 0; i < parameter_blocks.size(); ++i) {
				for (int j = i; j < parameter_blocks.size(); ++j) {
				    covariance_blocks.add(new Pair<double[], double[]>(parameter_blocks.get(i),
				                               parameter_blocks.get(j)));
				}
			}
			
			return pairCompute(covariance_blocks, problem);
		}

		
		public boolean pairCompute(Vector<Pair<double[], double[]>> covariance_blocks,
	                ProblemImpl problem) {
				boolean status = CheckForPairDuplicates(covariance_blocks);
				if (!status) {
					return false;
				}
				problem_ = problem;
				parameter_block_to_row_index_.clear();
				covariance_matrix_ = null;
				is_valid_ = (ComputeCovarianceSparsity(covariance_blocks, problem) &&
				  ComputeCovarianceValues());
				is_computed_ = true;
				return is_valid_;
		}
		
		boolean ComputeCovarianceValues() {
			  if (options_.algorithm_type == CovarianceAlgorithmType.DENSE_SVD) {
			    return ComputeCovarianceValuesUsingDenseSVD();
			  }

			  if (options_.algorithm_type == CovarianceAlgorithmType.SPARSE_QR) {
					  System.err.println("CovarianceAlgorithmType.SPARSE_QR not supported");
					  return false;
			  }
			    
			  System.err.println("options_.algorithm_type = " + options_.algorithm_type + " is illegal");
			  return false;
		}
		
		boolean ComputeCovarianceValuesUsingDenseSVD() {
			  int i;
			  EventLogger event_logger = new EventLogger("CovarianceImpl::ComputeCovarianceValuesUsingDenseSVD");
			  if (covariance_matrix_ == null) {
			    // Nothing to do, all zeros covariance matrix.
			    return true;
			  }

			  CRSMatrix jacobian = new CRSMatrix();
			  

			  problem_.Evaluate(evaluate_options_, null, null, null, jacobian);
			  event_logger.AddEvent("Evaluate");

			  Matrix dense_jacobian = new Matrix(jacobian.num_rows, jacobian.num_cols, 0.0);
			  for (int r = 0; r < jacobian.num_rows; ++r) {
			    for (int idx = jacobian.rows.get(r); idx < jacobian.rows.get(r + 1); ++idx) {
			      final int c = jacobian.cols.get(idx);
			      dense_jacobian.set(r, c, jacobian.values.get(idx));
			    }
			  }
			  event_logger.AddEvent("ConvertToDenseMatrix");

			  int m = dense_jacobian.getRowDimension();
			  int n = dense_jacobian.getColumnDimension();
			  double A[][] = dense_jacobian.getArray();
			  double singular_values[] = new double[Math.min(m,n)];
			  int ldu = m;
		      double U[][] = new double[ldu][Math.min(m,n)];
		      int ldvt = Math.min(m,n);
		      double VT[][] = new double[ldvt][n];
		      double work[] = new double[1];
		      int lwork = -1;
		      int info[] = new int[1];
			  svd.dgesvd('S','S',m,n,A,m,singular_values,U,ldu,VT,ldvt,work,lwork,info);
			  lwork = (int)work[0];
		      work = new double[lwork];
		      svd.dgesvd('S','S',m,n,A,m,singular_values,U,ldu,VT,ldvt,work,lwork,info);
		      if (info[0] < 0) {
              	System.err.println("In svd.dgesvd argument " + (-info[0]) + " had an illegal value");
              	Preferences.debug("In svd.dgesvd argument " + (-info[0]) + " had an illegal value\n", Preferences.DEBUG_ALGORITHM);
              	return false;
              }
	          if (info[0] > 0) {
              	System.err.println("In svd.dgesvd dbdsqr did not converge.");
              	Preferences.debug("In svd.dgesvd dbdsqr did not converge.\n",
              			Preferences.DEBUG_ALGORITHM);
              	return false;
              }
			  
			  event_logger.AddEvent("SingularValueDecomposition");

			  final int num_singular_values = singular_values.length;
			  double inverse_squared_singular_values[] = new double[num_singular_values];

			  final double max_singular_value = singular_values[0];
			  final double min_singular_value_ratio =
			      Math.sqrt(options_.min_reciprocal_condition_number);

			  final boolean automatic_truncation = (options_.null_space_rank < 0);
			  final int max_rank = Math.min(num_singular_values,
			                                num_singular_values - options_.null_space_rank);

			  // Compute the squared inverse of the singular values. Truncate the
			  // computation based on min_singular_value_ratio and
			  // null_space_rank. When either of these two quantities are active,
			  // the resulting covariance matrix is a Moore-Penrose inverse
			  // instead of a regular inverse.
			  for (i = 0; i < max_rank; ++i) {
			    double singular_value_ratio = singular_values[i] / max_singular_value;
			    if (singular_value_ratio < min_singular_value_ratio) {
			      // Since the singular values are in decreasing order, if
			      // automatic truncation is enabled, then from this point on
			      // all values will fail the ratio test and there is nothing to
			      // do in this loop.
			      if (automatic_truncation) {
			        break;
			      } else {
			        System.err.println("Error: Covariance matrix is near rank deficient ");
			        System.err.println("and the user did not specify a non-zero");
			        System.err.println("Covariance::Options::null_space_rank ");
			        System.err.println("to enable the computation of a Pseudo-Inverse. ");
			        System.err.println("Reciprocal condition number: " + (singular_value_ratio * singular_value_ratio));
			        System.err.println("min_reciprocal_condition_number: " + options_.min_reciprocal_condition_number);
			        return false;
			      }
			    }

			    inverse_squared_singular_values[i] =
			        1.0 / (singular_values[i] * singular_values[i]);
			  }

			  Matrix matrixVT = new Matrix(VT);
			  Matrix matrixV = matrixVT.transpose();
			  double singularArr[][] = new double[num_singular_values][num_singular_values];
			  for (i = 0; i < num_singular_values; i++) {
			      singularArr[i][i] = inverse_squared_singular_values[i];  
			  }
			  Matrix singularMat = new Matrix(singularArr);
			  Matrix dense_covariance =  (matrixV.times(singularMat)).times(matrixVT);
			      
			  event_logger.AddEvent("PseudoInverse");

			  final int num_rows = covariance_matrix_.num_rows();
			  final int[] rows = covariance_matrix_.rows();
			  final int[] cols = covariance_matrix_.cols();
			  double[] values = covariance_matrix_.mutable_values();

			  for (int r = 0; r < num_rows; ++r) {
			    for (int idx = rows[r]; idx < rows[r + 1]; ++idx) {
			      final int c = cols[idx];
			      values[idx] = dense_covariance.get(r, c);
			    }
			  }
			  event_logger.AddEvent("CopyToCovarianceMatrix");
			  return true;
			}


			
		
		// Determine the sparsity pattern of the covariance matrix based on
		// the block pairs requested by the user.
		public boolean ComputeCovarianceSparsity(
		    Vector<Pair<double[], double[]>>  original_covariance_blocks,
		    ProblemImpl problem) {
		  int i,j;
		  EventLogger event_logger = new EventLogger("CovarianceImpl::ComputeCovarianceSparsity");

		  // Determine an ordering for the parameter block, by sorting the
		  // parameter blocks by their pointers.
		  Vector<double[]> all_parameter_blocks = new Vector<double[]>();
		  problem.GetParameterBlocks(all_parameter_blocks);
		  HashMap<double[], ParameterBlock> parameter_map = problem.parameter_map();
		  HashSet<ParameterBlock> parameter_blocks_in_use = new HashSet<ParameterBlock>();
		  Vector<ResidualBlock> residual_blocks = new Vector<ResidualBlock>();
		  problem.GetResidualBlocks(residual_blocks);

		  for (i = 0; i < residual_blocks.size(); ++i) {
		    ResidualBlock residual_block = residual_blocks.get(i);
		    for (j = 0; j < residual_block.NumParameterBlocks(); j++) {
		        parameter_blocks_in_use.add(residual_block.parameter_blocks()[j]);
		    }
		  }

		  constant_parameter_blocks_.clear();
		  Vector<double[]> active_parameter_blocks =
		      evaluate_options_.parameter_blocks;
		  active_parameter_blocks.clear();
		  for (i = 0; i < all_parameter_blocks.size(); ++i) {
		    double[] parameter_block = all_parameter_blocks.get(i);
		    ParameterBlock block = parameter_map.get(parameter_block);
		    if (block == null) {
		    	System.err.println("In ComputeCovarianceSparsity parameter_map.get(parameter_block) returned null");
		    	return false;
		    }
		    /*boolean contains_block = false;
		    for (ParameterBlock pb: parameter_blocks_in_use) {
		    	if (pb.equalsParameterBlock(block)) {
		    		contains_block = true;
		    	}
		    }*/
		    if (!block.IsConstant() && (parameter_blocks_in_use.contains(block))) {
		    //if (!block.IsConstant() && contains_block) {
		      active_parameter_blocks.add(parameter_block);
		    } else {
		      constant_parameter_blocks_.add(parameter_block);
		    }
		  }
		  
		  ArrayList<indexArrayItem> ia = new ArrayList<indexArrayItem>();
			for (i = 0; i < active_parameter_blocks.size(); i++) {
				ia.add(new indexArrayItem(i,active_parameter_blocks.get(i)));
			}
			indexArrayComparator ic = new indexArrayComparator();
			Collections.sort(ia, ic);
			active_parameter_blocks.clear();
			for (i = 0; i < ia.size(); i++) {
				active_parameter_blocks.add(ia.get(i).getArray());
			}

		  // Compute the number of rows.  Map each parameter block to the
		  // first row corresponding to it in the covariance matrix using the
		  // ordering of parameter blocks just constructed.
		  int num_rows = 0;
		  parameter_block_to_row_index_.clear();
		  for (i = 0; i < active_parameter_blocks.size(); ++i) {
		    double[] parameter_block = active_parameter_blocks.get(i);
		    final int parameter_block_size =
		        problem.ParameterBlockLocalSize(parameter_block);
		    parameter_block_to_row_index_.put(parameter_block,num_rows);
		    num_rows += parameter_block_size;
		  }

		  // Compute the number of non-zeros in the covariance matrix.  Along
		  // the way flip any covariance blocks which are in the lower
		  // triangular part of the matrix.
		  int num_nonzeros = 0;
		  Vector<Pair<double[], double[]>> covariance_blocks = new Vector<Pair<double[], double[]>>();
		  for (i = 0; i <  original_covariance_blocks.size(); ++i) {
		    Pair<double[], double[]> block_pair =
		        original_covariance_blocks.get(i);
		    if (constant_parameter_blocks_.contains(block_pair.first) ||
		        constant_parameter_blocks_.contains(block_pair.second)) {
		      continue;
		    }

		    int index1, index2;
		    if (parameter_block_to_row_index_.get(block_pair.first) == null) {
		    	System.err.println("In ComputeCovarianceSparsity parameter_block_to_row_index_.get(block_pair.first) == null");
		    	return false;
		    }
		    else {
		    	index1 = parameter_block_to_row_index_.get(block_pair.first);
		    }
		    if (parameter_block_to_row_index_.get(block_pair.second) == null) {
		    	System.err.println("In ComputeCovarianceSparsity parameter_block_to_row_index_.get(block_pair.second) == null");
		    	return false;
		    }
		    else {
		    	index2 = parameter_block_to_row_index_.get(block_pair.second);
		    }
		    final int size1 = problem.ParameterBlockLocalSize(block_pair.first);
		    final int size2 = problem.ParameterBlockLocalSize(block_pair.second);
		    num_nonzeros += size1 * size2;

		    // Make sure we are constructing a block upper triangular matrix.
		    if (index1 > index2) {
		      covariance_blocks.add(new Pair<double[], double[]>(block_pair.second,
		                                            block_pair.first));
		    } else {
		      covariance_blocks.add(block_pair);
		    }
		  }

		  if (covariance_blocks.size() == 0) {
			if (2 <= MAX_LOG_LEVEL) {
		        Preferences.debug("No non-zero covariance blocks found\n", Preferences.DEBUG_ALGORITHM);
			}
		    covariance_matrix_ = null;
		    return true;
		  }

		  // Sort the block pairs. As a consequence we get the covariance
		  // blocks as they will occur in the CompressedRowSparseMatrix that
		  // will store the covariance.
		  ArrayList<indexArrayArrayItem> iaa = new ArrayList<indexArrayArrayItem>();
			for (i = 0; i < covariance_blocks.size(); i++) {
				iaa.add(new indexArrayArrayItem(i,covariance_blocks.get(i).getFirst(),covariance_blocks.get(i).getSecond()));
			}
			indexArrayArrayComparator icc = new indexArrayArrayComparator();
			Collections.sort(iaa, icc);
			covariance_blocks.clear();
			for (i = 0; i < iaa.size(); i++) {
				covariance_blocks.add(new Pair<double[], double[]>(iaa.get(i).getArray1(), iaa.get(i).getArray2()));
			}

		  // Fill the sparsity pattern of the covariance matrix.\
		  covariance_matrix_ = new CompressedRowSparseMatrix(num_rows, num_rows, num_nonzeros);

		  int[] rows = covariance_matrix_.mutable_rows();
		  int[] cols = covariance_matrix_.mutable_cols();

		  // Iterate over parameter blocks and in turn over the rows of the
		  // covariance matrix. For each parameter block, look in the upper
		  // triangular part of the covariance matrix to see if there are any
		  // blocks requested by the user. If this is the case then fill out a
		  // set of compressed rows corresponding to this parameter block.
		  //
		  // The key thing that makes this loop work is the fact that the
		  // row/columns of the covariance matrix are ordered by the pointer
		  // values of the parameter blocks. Thus iterating over the keys of
		  // parameter_block_to_row_index_ corresponds to iterating over the
		  // rows of the covariance matrix in order.
		  i = 0;  // index into covariance_blocks.
		  int cursor = 0;  // index into the covariance matrix.
		    Collection<Integer> intValues = parameter_block_to_row_index_.values();
			Iterator<Integer> intValues_it = intValues.iterator();
			Set<double[]> darraySet = parameter_block_to_row_index_.keySet();
			Iterator<double[]> darray_iterator = darraySet.iterator();
			ArrayList<indexIntegerdoubleArrayItem> iad = new ArrayList<indexIntegerdoubleArrayItem>();
			while (darray_iterator.hasNext()) {
				final double[] row_block = darray_iterator.next();
				int row_begin = intValues_it.next();
				iad.add(new indexIntegerdoubleArrayItem(i,row_begin, row_block));
			} 
			indexIntegerdoubleArrayComparator icd = new indexIntegerdoubleArrayComparator();
			Collections.sort(iad, icd);
			for (int m = 0; m < iad.size(); m++) {
				double[] row_block = iad.get(m).getArray();
				final int row_block_size = problem.ParameterBlockLocalSize(row_block);
				int row_begin = iad.get(m).getRowBegin();
		    // Iterate over the covariance blocks contained in this row block
		    // and count the number of columns in this row block.
		    int num_col_blocks = 0;
		    //int num_columns = 0;
		    for (j = i; j < covariance_blocks.size(); ++j, ++num_col_blocks) {
		      final Pair<double[], double[]> block_pair =
		          covariance_blocks.get(j);
		      if (block_pair.first != row_block) {
		        break;
		      }
		      //num_columns += problem.ParameterBlockLocalSize(block_pair.second);
		    }

		    // Fill out all the compressed rows for this parameter block.
		    for (int r = 0; r < row_block_size; ++r) {
		      rows[row_begin + r] = cursor;
		      for (int c = 0; c < num_col_blocks; ++c) {
		        final double[] col_block = covariance_blocks.get(i + c).second;
		        final int col_block_size = problem.ParameterBlockLocalSize(col_block);
		        int col_begin;
		        if (parameter_block_to_row_index_.get(col_block) == null) {
		            System.err.println("In ComputeCovarianceSparisty parameter_block_to_row_index_.get(col_block) == null");
		            return false;
		        }
		        else {
		        	col_begin = parameter_block_to_row_index_.get(col_block);
		        }
		        for (int k = 0; k < col_block_size; ++k) {
		          cols[cursor++] = col_begin++;
		        }
		      }
		    }

		    i+= num_col_blocks;
		  }

		  rows[num_rows] = cursor;
		  
		  return true;
		}
		
		public boolean GetCovarianceBlock(double[] parameter_block1,
                double[] parameter_block2,
                double[] covariance_block) {
                return GetCovarianceBlockInTangentOrAmbientSpace(parameter_block1,
                                      parameter_block2,
                                      true,  // ambient
                                      covariance_block);
        }
		
		public boolean GetCovarianceBlockInTangentSpace(
			    double[] parameter_block1,
			    double[] parameter_block2,
			    double[] covariance_block) {
			     return GetCovarianceBlockInTangentOrAmbientSpace(parameter_block1,
			                                                          parameter_block2,
			                                                          false,  // tangent
                                                                  covariance_block);
		}
		
		public boolean GetCovarianceBlockInTangentOrAmbientSpace(
			    double[] original_parameter_block1,
			    double[] original_parameter_block2,
			    boolean lift_covariance_to_ambient_space,
			    double[] covariance_block) {
			    int i, r, c;
			    if (!is_computed_) {
			      System.err.println("Covariance::GetCovarianceBlockInTangentOrAmbientSpace called before Covariance::Compute");
			      return false;
			    }
			    if (!is_valid_) {
			      System.err.println("Covariance::GetCovarianceBlockInTangentOrAmbientSpace called when Covariance::Compute returned false.");
			      return false;
		        }

			  // If either of the two parameter blocks is constant, then the
			  // covariance block is also zero.
			  if (constant_parameter_blocks_.contains(original_parameter_block1) ||
			      constant_parameter_blocks_.contains(original_parameter_block2)) {
			    final HashMap<double[], ParameterBlock> parameter_map = problem_.parameter_map();
			    ParameterBlock block1 = parameter_map.get(original_parameter_block1);
			    if (block1 == null) {
			    	System.err.println("In GetCovarianceBlockInTangentOrAmbientSpace parameter_map.get(original_parameter_block1) returned null");
			    	return false;
			    }
			   
			    ParameterBlock block2 = parameter_map.get(original_parameter_block2);
			    if (block2 == null) {
			    	System.err.println("In GetCovarianceBlockInTangentOrAmbientSpace parameter_map.get(original_parameter_block2) returned null");
			    	return false;
			    } 

			    final int block1_size = block1.Size();
			    final int block2_size = block2.Size();
			    final int block1_local_size = block1.LocalSize();
			    final int block2_local_size = block2.LocalSize();
			    if (!lift_covariance_to_ambient_space) {
			      for (i = 0; i < block1_local_size * block2_local_size; i++) {
			    	  covariance_block[i] = 0;
			      }
			    } else {
			      for (i = 0; i < block1_size * block2_size; i++) {
			    	  covariance_block[i] = 0;
			      }
			    }
			    return true;
			  }

			  double[] parameter_block1 = original_parameter_block1;
			  double[] parameter_block2 = original_parameter_block2;
			  boolean transpose = false;
			  if (parameter_block1.length > parameter_block2.length) {
				  transpose = true;
			  }
			  else if (parameter_block1.length == parameter_block2.length) {
				  for (i = 0; i < parameter_block1.length; i++) {
					  if (parameter_block1[i] > parameter_block2[i]) {
						  transpose = true;
						  break;
					  }
					  else if (parameter_block1[i] < parameter_block2[i]) {
						  break;
					  }
				  }
			  }
			  if (transpose) {
				double temp[];
				temp = parameter_block1;
				parameter_block1 = parameter_block2;
				parameter_block2 = temp;
			  }

			  // Find where in the covariance matrix the block is located.
			  Integer row_begin_Integer = parameter_block_to_row_index_.get(parameter_block1);
			  if (row_begin_Integer == null) {
				  System.err.println("In GetCovarianceBlockInTangentOrAmbientSpace parameter_block_to_row_index_.get(parameter_block1) returned null");
				  return false;
			  }
			  final int row_begin = row_begin_Integer.intValue();
			  Integer col_begin_Integer = parameter_block_to_row_index_.get(parameter_block2);
			  if (col_begin_Integer == null) {
				  System.err.println("In GetCovarianceBlockInTangentOrAmbientSpace parameter_block_to_row_index_.get(parameter_block2) returned null");
				  return false;
			  }
			  final int col_begin = col_begin_Integer.intValue();
			  final int[] rows = covariance_matrix_.rows();
			  final int[] cols = covariance_matrix_.cols();
			  final int row_size = rows[row_begin + 1] - rows[row_begin];
			  int cols_begin_length = cols.length - rows[row_begin];
			  int cols_begin[] = new int[cols_begin_length];
			  for (i = 0; i < cols_begin_length; i++) {
				  cols_begin[i] = cols[rows[row_begin] + i];
			  }

			  // The only part that requires work is walking the compressed column
			  // vector to determine where the set of columns correspnding to the
			  // covariance block begin.
			  int offset = 0;
			  while (cols_begin[offset] != col_begin && offset < row_size) {
			    ++offset;
			  }

			  if (offset == row_size) {
			    System.err.println("Unable to find covariance block for:");
			    for (i = 0; i < original_parameter_block1.length; i++) {
			    	System.err.println("original_parameter_block1[" + i +"] = " + original_parameter_block1[i]);
			    }
			    for (i = 0; i < original_parameter_block2.length; i++) {
			    	System.err.println("original_parameter_block2[" + i +"] = " + original_parameter_block2[i]);
			    }
			    return false;
			  }

			  final HashMap<double[], ParameterBlock> parameter_map = problem_.parameter_map();
			  ParameterBlock block1 = parameter_map.get(parameter_block1);
			    if (block1 == null) {
			    	System.err.println("In GetCovarianceBlockInTangentOrAmbientSpace parameter_map.get(parameter_block1) returned null");
			    	return false;
			    }
			   
			    ParameterBlock block2 = parameter_map.get(parameter_block2);
			    if (block2 == null) {
			    	System.err.println("In GetCovarianceBlockInTangentOrAmbientSpace parameter_map.get(parameter_block2) returned null");
			    	return false;
			    } 
			 
			  final LocalParameterization local_param1 = block1.local_parameterization();
			  final LocalParameterization local_param2 = block2.local_parameterization();
			  final int block1_size = block1.Size();
			  final int block1_local_size = block1.LocalSize();
			  final int block2_size = block2.Size();
			  final int block2_local_size = block2.LocalSize();

			  //ConstMatrixRef cov(covariance_matrix_->values() + rows[row_begin],
			                     //block1_size,
			                     //row_size);
			  // Changed from original block1_size to block1_local_size because in ComputeCovarianceSparsity:
			  // final int size1 = problem.ParameterBlockLocalSize(block_pair.first);
			  // final int size2 = problem.ParameterBlockLocalSize(block_pair.second);
			  // num_nonzeros += size1 * size2;
              // covariance_matrix_ = new CompressedRowSparseMatrix(num_rows, num_rows, num_nonzeros);
			  // and class CompressedRowSparseMatrix has:
			  // public CompressedRowSparseMatrix(int num_rows, int num_cols, int max_num_nonzeros) {
			  // values_ = new double[max_num_nonzeros];
			  // so values_ can only have a length given by local sizes.
			  double cov[][] = new double[block1_local_size][row_size];
			  for (i = 0, r = 0; r < block1_local_size; r++) {
				  for (c = 0; c < row_size; c++, i++) {
					  cov[r][c] = covariance_matrix_.values()[i + rows[row_begin]];
				  }
			  }

			  // Fast path when there are no local parameterizations or if the
			  // user does not want it lifted to the ambient space.
			  if ((local_param1 == null && local_param2 == null) ||
			      !lift_covariance_to_ambient_space) {
			    if (transpose) {
			          for (r = 0; r < block2_local_size; r++) {
			        	  for (c = 0; c < block1_local_size; c++) {
			        		  covariance_block[r*block1_local_size + c] = cov[c][r+offset];
			        	  }
			          }
			    } else {
			          for (r = 0; r < block1_local_size; r++) {
			        	  for (c = 0; c < block2_local_size; c++) {
			        		  covariance_block[r * block2_local_size + c] = cov[r][c + offset];
			        	  }
			          }
			    }
			    return true;
			  }

			  // If local parameterizations are used then the covariance that has
			  // been computed is in the tangent space and it needs to be lifted
			  // back to the ambient space.
			  //
			  // This is given by the formula
			  //
			  //  C'_12 = J_1 C_12 J_2'
			  //
			  // Where C_12 is the local tangent space covariance for parameter
			  // blocks 1 and 2. J_1 and J_2 are respectively the local to global
			  // jacobians for parameter blocks 1 and 2.
			  //
			  // See Result 5.11 on page 142 of Hartley & Zisserman (2nd Edition)
			  // for a proof.
			  //
			  // TODO(sameeragarwal): Add caching of local parameterization, so
			  // that they are computed just once per parameter block.
			  int minLength;
			  double block1_jacobian[][] = new double[block1_size][block1_local_size];
			  if (local_param1 == null) {
				minLength = Math.min(block1_size, block1_local_size);
				for (i = 0; i < minLength; i++) {
					block1_jacobian[i][i] = 1.0;
				}
			  } else {
			    local_param1.ComputeJacobian(parameter_block1, 0, block1_jacobian);
			  }

			  double block2_jacobian[][] = new double[block2_size][block2_local_size];
			  // Fast path if the user is requesting a diagonal block.
			  if (parameter_block1 == parameter_block2) {
			    block2_jacobian = block1_jacobian;
			  } else {
			    if (local_param2 == null) {
			      minLength = Math.min(block2_size, block2_local_size);
			      for (i = 0; i < minLength; i++) {
			    	  block2_jacobian[i][i] = 1.0;
			      }
			    } else {
			      local_param2.ComputeJacobian(parameter_block2, 0, block2_jacobian);
			    }
			  }

			  Matrix block1_jacobianMat = new Matrix(block1_jacobian);
			  Matrix block2_jacobianMat = new Matrix(block2_jacobian);
			  Matrix cov_blockMat = new Matrix(block1_local_size, block2_local_size);
			  for (r = 0; r < block1_local_size; r++) {
				  for (c = 0; c < block2_local_size; c++) {
					  cov_blockMat.set(r, c, cov[r][c+offset]);
				  }
			  }
			  Matrix multMat;
			  if (transpose) {
				multMat = (block2_jacobianMat.times(cov_blockMat.transpose())).times(block1_jacobianMat.transpose());
				for (i = 0, r = 0; r < block2_size; r++) {
					for (c = 0; c < block1_size; c++, i++) {
						covariance_block[i] = multMat.get(r,c);
					}
				}
			  } else {
				multMat = (block1_jacobianMat.times(cov_blockMat)).times(block2_jacobianMat.transpose());
				for (i = 0, r = 0; r < block1_size; r++) {
					for (c = 0; c < block2_size; c++, i++) {
						covariance_block[i] = multMat.get(r,c);
					}
				}
			  }

			  return true;
			}
		
		public boolean GetCovarianceMatrix(
			    Vector<double[]> parameter_blocks,
			    double[] covariance_matrix) {
			  return GetCovarianceMatrixInTangentOrAmbientSpace(parameter_blocks,
			                                                           true,  // ambient
			                                                           covariance_matrix);
		}
		
		public boolean GetCovarianceMatrixInTangentSpace(
			    Vector<double []> parameter_blocks,
			    double []covariance_matrix) {
			  return GetCovarianceMatrixInTangentOrAmbientSpace(parameter_blocks,
			                                                           false,  // tangent
			                                                           covariance_matrix);
			}




		public boolean GetCovarianceMatrixInTangentOrAmbientSpace(
			    Vector<double[]> parameters,
			    boolean lift_covariance_to_ambient_space,
			    double[] covariance_matrix) {
			    int i,j,r,c;
			    int ia[] = new int[1];
			    int ja[] = new int[1];
			    if (!is_computed_) {
			      System.err.println("Covariance::GetCovarianceMatrixInTangentOrAmbientSpace called before Covariance::Compute");
			      return false;
			    }
			    if (!is_valid_) {
			      System.err.println("Covariance::GetCovarianceMatrixInTangentOrAmbientSpace called when Covariance::Compute returned false.");
			      return false;
		        }

			  final HashMap<double[], ParameterBlock> parameter_map = problem_.parameter_map();
			  // For OpenMP compatibility we need to define these vectors in advance
			  final int num_parameters = parameters.size();
			  Vector<Integer> parameter_sizes = new Vector<Integer>(num_parameters);
			  Vector<Integer> cum_parameter_size = new Vector<Integer>();
			  for (i = 0; i < num_parameters + 1; i++) {
				  cum_parameter_size.add(0);
			  }
			  
			  for (i = 0; i < num_parameters; ++i) {
				  ParameterBlock block = parameter_map.get(parameters.get(i));
				  if (block == null) {
				    	System.err.println("In GetCovarianceMatrixInTangentOrAmbientSpace parameter_map.get(parameters.get(i)) returned null");
				    	return false;
				  }
			    if (lift_covariance_to_ambient_space) {
			      parameter_sizes.add(block.Size());
			    } else {
			      parameter_sizes.add(block.LocalSize());
			    }
			  }
			  for (j = 0; j < parameter_sizes.size(); j++) {
				  cum_parameter_size.set(j+1, cum_parameter_size.get(j) + parameter_sizes.get(j));
			  }
			  int max_covariance_block_size = 0;
			  for (j = 0; j < parameter_sizes.size(); j++) {
				  if (parameter_sizes.get(j) > max_covariance_block_size) {
			          max_covariance_block_size = parameter_sizes.get(j);  
				  }
			  }
			  final int covariance_size = cum_parameter_size.lastElement();

			  // Assemble the blocks in the covariance matrix.
			  //MatrixRef covariance(covariance_matrix, covariance_size, covariance_size);
			  double covariance[][] = new double[covariance_size][covariance_size];
			  for (i = 0, r = 0; r < covariance_size; r++) {
		          for (c = 0; c < covariance_size; c++, i++) {
		        	  covariance[r][c] = covariance_matrix[i];
		          }
			  }
			  //const int num_threads = options_.num_threads;
			  final int num_threads = 1;
			  double workspace[] =
			      new double[num_threads * max_covariance_block_size *
			                 max_covariance_block_size];

			  boolean success = true;

			//#if !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))
			  //ThreadTokenProvider thread_token_provider(num_threads);
			//#endif // !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))

			  // Technically the following code is a double nested loop where
			  // i = 1:n, j = i:n.
			  int iteration_count = (num_parameters * (num_parameters + 1)) / 2;
			//#if defined(CERES_USE_OPENMP)
			//#    pragma omp parallel for num_threads(num_threads) schedule(dynamic)
			//#endif // CERES_USE_OPENMP
			//#if !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))
			  for (int k = 0; k < iteration_count; ++k) {
			//#else
			  //problem_->context()->EnsureMinimumThreads(num_threads);
			  //ParallelFor(problem_->context(),
			              //0,
			              //iteration_count,
			              //num_threads,
			              //[&](int thread_id, int k) {
			//#endif // !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))
			      LinearIndexToUpperTriangularIndex(k, num_parameters, ia, ja);

			      int covariance_row_idx = cum_parameter_size.get(ia[0]);
			      int covariance_col_idx = cum_parameter_size.get(ja[0]);
			      int size_i = parameter_sizes.get(ia[0]);
			      int size_j = parameter_sizes.get(ja[0]);
			//#if !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))
			      //const ScopedThreadToken scoped_thread_token(&thread_token_provider);
			      //const int thread_id = scoped_thread_token.token();
			//#endif // !(defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS))
			      //double* covariance_block =
			          //workspace.get() +
			          //thread_id * max_covariance_block_size * max_covariance_block_size;
			      double[] covariance_block = workspace;
			      if (!GetCovarianceBlockInTangentOrAmbientSpace(
			              parameters.get(ia[0]), parameters.get(ja[0]), lift_covariance_to_ambient_space,
			              covariance_block)) {
			        success = false;
			      }

			      //covariance.block(covariance_row_idx, covariance_col_idx,
			                       //size_i, size_j) =
			          //MatrixRef(covariance_block, size_i, size_j);
			      for (r = 0; r < size_i; r++) {
			    	  for (c = 0; c < size_j; c++) {
			    		  covariance[r + covariance_row_idx][c + covariance_col_idx] = covariance_block[r*size_j + c];
			    	  }
			      }

			      if (ia[0] != ja[0]) {
			        //covariance.block(covariance_col_idx, covariance_row_idx,
			                         //size_j, size_i) =
			            //MatrixRef(covariance_block, size_i, size_j).transpose();
			        for (r = 0; r < size_i; r++) {
			        	for (c = 0; c < size_j; c++) {
			        		covariance[c + covariance_col_idx][r + covariance_row_idx] = covariance_block[r*size_j + c];
			        	}
			        }

			      }
			      
			      for (i = 0, r = 0; r < covariance_size; r++) {
			          for (c = 0; c < covariance_size; c++, i++) {
			        	  covariance_matrix[i] = covariance[r][c];
			          }
				  }
			    }
			//#if defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
			   //);
			//#endif // defined(CERES_USE_TBB) || defined(CERES_USE_CXX11_THREADS)
			  return success;
			}


		void LinearIndexToUpperTriangularIndex(int k, int n, int i[], int j[]) {
			  // This works by unfolding a rectangle into a triangle.
			  // Say n is even. 4 is a nice even number. The 10 i,j pairs that we
			  // want to produce are:
			  // 0,0 0,1 0,2 0,3
			  //     1,1 1,2 1,3
			  //         2,2 2,3
			  //             3,3
			  // This triangle can be folded into a 5x2 rectangle:
			  // 3,3 0,0 0,1 0,2 0,3
			  // 2,2 2,3 1,1 1,2 1,3

			  // If N is odd, say 5, then the 15 i,j pairs are:
			  // 0,0 0,1 0,2 0,3 0,4
			  //     1,1 1,2 1,3 1,4
			  //         2,2 2,3 2,3
			  //             3,3 3,4
			  //                 4,4
			  // which folds to a 5x3 rectangle:
			  // 0,0 0,1 0,2 0,3 0,4
			  // 4,4 1,1 1,2 1,3 1,4
			  // 3,3 3,4 2,2 2,3 2,4

			  // All this function does is map the linear iteration position to a
			  // location in the rectangle and work out the appropriate (i, j) for that
			  // location.
			  if ((n & 1) != 0) {
			    // Odd n. The tip of the triangle is on row 1.
			    int w = n;  // Width of the rectangle to unfold
			    int i0 = k / w;
			    int j0 = k % w;
			    if (j0 >= i0) {
			      i[0] = i0;
			      j[0] = j0;
			    } else {
			      i[0] = n - i0;
			      j[0] = i[0] + j0;
			    }
			  } else {
			    // Even n. The tip of the triangle is on row 0, making it one wider.
			    int w = n + 1;
			    int i0 = k / w;
			    int j0 = k % w;
			    if (j0 > i0) {
			      i[0] = i0;
			      j[0] = j0 - 1;
			    } else {
			      i[0] = n - 1 - i0;
			      j[0] = i[0] + j0;
			    }
			  }
			}


    }
	  
	// Find the connected component for a vertex implemented using the
	// find and update operation for disjoint-set. Recursively traverse
	// the disjoint set structure till you reach a vertex whose connected
	// component has the same id as the vertex itself. Along the way
	// update the connected components of all the vertices. This updating
	// is what gives this data structure its efficiency.
	public <Vertex> Vertex FindConnectedComponent(Vertex vertex,
	                              HashMap<Vertex, Vertex> union_find) {
	  Vertex second = union_find.get(vertex);
	  if (second == null) {
		  System.err.println("Null value for vertex key in FindConnectedComponent");
		  return null;
	  }
	  if (second != vertex) {
	    second = FindConnectedComponent(second, union_find);
	  }

	  return second;
	}
	
	class SingleLinkageClusteringOptions {
		   // Graph edges with edge weight less than min_similarity are ignored
		  // during the clustering process.
		  public double min_similarity;
		  public SingleLinkageClusteringOptions() {
		      min_similarity = 0.99;
		  }
  
	};
	
	// Compute a partitioning of the vertices of the graph using the
	// single linkage clustering algorithm. Edges with weight less than
	// SingleLinkageClusteringOptions::min_similarity will be ignored.
	//
	// membership upon return will contain a mapping from the vertices of
	// the graph to an integer indicating the identity of the cluster that
	// it belongs to.
	//
	// The return value of this function is the number of clusters
	// identified by the algorithm.
	public int ComputeSingleLinkageClustering(
	    SingleLinkageClusteringOptions options,
	    WeightedGraph<Integer> graph,
	    HashMap<Integer, Integer> membership) {
		if (membership  == null) {
			System.err.println("HashMap<Integer, Integer> membership == null in ComputeSingleLinkageClustering");
			return -1;
		}
		membership.clear();

		  // Initially each vertex is in its own cluster.
		  final HashSet<Integer> vertices = graph.vertices();
		  Iterator<Integer> it = vertices.iterator();
		  while (it.hasNext()) {
			  Integer IntNext = it.next();
		      membership.put(IntNext, IntNext);
		  }
		  

		  Iterator<Integer> it1 = vertices.iterator();
		  while (it1.hasNext()) {
			final int vertex1 = it1.next();
		    final HashSet<Integer> neighbors = graph.Neighbors(vertex1);
		    Iterator<Integer> it2 = neighbors.iterator();
		    while (it2.hasNext()) {
		      final int vertex2 = it2.next();

		      // Since the graph is undirected, only pay attention to one side
		      // of the edge and ignore weak edges.
		      if ((vertex1 > vertex2) ||
		          (graph.EdgeWeight(vertex1, vertex2) < options.min_similarity)) {
		        continue;
		      }

		      // Use a union-find algorithm to keep track of the clusters.
		      final int c1 = FindConnectedComponent(vertex1, membership);
		      final int c2 = FindConnectedComponent(vertex2, membership);

		      if (c1 == c2) {
		        continue;
		      }

		      if (c1 < c2) {
		        membership.put(c2,c1);
		      } else {
		        membership.put(c1,c2);
		      }
		    }
		  }

		  // Make sure that every vertex is connected directly to the vertex
		  // identifying the cluster.
		  int num_clusters = 0;
		  Set<Integer> keySet = membership.keySet();
		  Iterator<Integer> key_iterator = keySet.iterator();
		  while (key_iterator.hasNext()) {
			int nextKey = key_iterator.next();
		    int nextValue = FindConnectedComponent(key_iterator.next(), membership);
		    if (nextKey == nextValue) {
		      ++num_clusters;
		    }
		  }

		  return num_clusters;

	}
	
	// Compute a degree two constrained Maximum Spanning Tree/forest of
	// the input graph. Caller owns the result.
	//
	// Finding degree 2 spanning tree of a graph is not always
	// possible. For example a star graph, i.e. a graph with n-nodes
	// where one node is connected to the other n-1 nodes does not have
	// a any spanning trees of degree less than n-1.Even if such a tree
	// exists, finding such a tree is NP-Hard.

	// We get around both of these problems by using a greedy, degree
	// constrained variant of Kruskal's algorithm. We start with a graph
	// G_T with the same vertex set V as the input graph G(V,E) but an
	// empty edge set. We then iterate over the edges of G in decreasing
	// order of weight, adding them to G_T if doing so does not create a
	// cycle in G_T} and the degree of all the vertices in G_T remains
	// bounded by two. This O(|E|) algorithm results in a degree-2
	// spanning forest, or a collection of linear paths that span the
	// graph G.
	public WeightedGraph<Integer> Degree2MaximumSpanningForest(WeightedGraph<Integer> graph) {
	  int i;
	  // Array of edges sorted in decreasing order of their weights.
	  Vector<Pair<Double, Pair<Integer, Integer> > > weighted_edges = new Vector<Pair<Double, Pair<Integer, Integer>>>();
	  WeightedGraph<Integer> forest = new WeightedGraph<Integer>();

	  // Disjoint-set to keep track of the connected components in the
	  // maximum spanning tree.
	  HashMap<Integer, Integer> disjoint_set = new HashMap<Integer, Integer>();

	  // Sort of the edges in the graph in decreasing order of their
	  // weight. Also add the vertices of the graph to the Maximum
	  // Spanning Tree graph and set each vertex to be its own connected
	  // component in the disjoint_set structure.
	  final HashSet<Integer> vertices = graph.vertices();
	  Iterator<Integer> it = vertices.iterator();
	  while (it.hasNext()) {
	    final int vertex1 = it.next();
	    forest.AddVertex(vertex1, graph.VertexWeight(vertex1));
	    disjoint_set.put(vertex1,vertex1);

	    final HashSet<Integer> neighbors = graph.Neighbors(vertex1);
	    Iterator<Integer> it2 = neighbors.iterator();
	    while (it2.hasNext()) {
	      final int vertex2 = it2.next();
	      if (vertex1 >= vertex2) {
	        continue;
	      }
	      final double weight = graph.EdgeWeight(vertex1, vertex2);
	      weighted_edges.add(
	          new Pair<Double, Pair<Integer,Integer>>(weight, new Pair<Integer, Integer>(vertex1, vertex2)));
	    }
	  }

	  // The elements of this vector, are pairs<edge_weight,
	  // edge>. Sorting it using the reverse iterators gives us the edges
	  // in decreasing order of edges.
	  ArrayList <weighted_edgesItem> weList = new ArrayList<weighted_edgesItem>();
	  for (i = 0; i < weighted_edges.size(); ++i) {
		  weList.add(new weighted_edgesItem(i,weighted_edges.get(i).second.first.intValue(),
				  weighted_edges.get(i).second.second.intValue(), weighted_edges.get(i).first));
	  }
	  Collections.sort(weList, new weighted_edgesComparator());
	  weighted_edges.clear();
	  for (i = 0; i < weList.size(); i++) {
		  weighted_edges.add(new Pair<Double, Pair<Integer, Integer>>(weList.get(i).getWeight(), new Pair<Integer, Integer>(weList.get(i).getVertex1(),
				  weList.get(i).getVertex2())));
	  }

	  // Greedily add edges to the spanning tree/forest as long as they do
	  // not violate the degree/cycle constraint.
	  for (i =0; i < weighted_edges.size(); ++i) {
	    final Pair<Integer, Integer> edge = weighted_edges.get(i).second;
	    final int vertex1 = edge.first;
	    final int vertex2 = edge.second;

	    // Check if either of the vertices are of degree 2 already, in
	    // which case adding this edge will violate the degree 2
	    // constraint.
	    if ((forest.Neighbors(vertex1).size() == 2) ||
	        (forest.Neighbors(vertex2).size() == 2)) {
	      continue;
	    }

	    // Find the id of the connected component to which the two
	    // vertices belong to. If the id is the same, it means that the
	    // two of them are already connected to each other via some other
	    // vertex, and adding this edge will create a cycle.
	    int root1 = FindConnectedComponent(vertex1, disjoint_set);
	    int root2 = FindConnectedComponent(vertex2, disjoint_set);

	    if (root1 == root2) {
	      continue;
	    }

	    // This edge can be added, add an edge in either direction with
	    // the same weight as the original graph.
	    final double edge_weight = graph.EdgeWeight(vertex1, vertex2);
	    forest.AddEdge(vertex1, vertex2, edge_weight);
	    forest.AddEdge(vertex2, vertex1, edge_weight);

	    // Connected the two connected components by updating the
	    // disjoint_set structure. Always connect the connected component
	    // with the greater index with the connected component with the
	    // smaller index. This should ensure shallower trees, for quicker
	    // lookup.
	    if (root2 < root1) {
	      int temp = root1;
	      root1 = root2;
	      root2 = temp;
	    }

	    disjoint_set.put(root2,root1);
	  }
	  return forest;
	}
	
	// Compare two vertices of a graph by their degrees, if the degrees
	// are equal then order them by their ids.
	class VertexTotalOrderingInteger {
	  private Graph<Integer> graph_;
	  public VertexTotalOrderingInteger(Graph<Integer> graph) {
	      graph_ = graph; 
	  }

	  public boolean operator(int lhs, int rhs) {
	    if (graph_.Neighbors(lhs).size() == graph_.Neighbors(rhs).size()) {
	      return lhs < rhs;
	    }
	    return graph_.Neighbors(lhs).size() < graph_.Neighbors(rhs).size();
	  }

	};
	
	class weighted_edgesItem {
		private int index;
		private int vertex1;
		private int vertex2;
		private double weight;
		
		public weighted_edgesItem(int index, int vertex1, int vertex2, double weight) {
			this.index = index;
			this.vertex1 = vertex1;
			this.vertex2 = vertex2;
			this.weight = weight;
		}
		
		public int getIndex() {
			return index;
		}
		
		public int getVertex1() {
			return vertex1;
		}
		
		public int getVertex2() {
		    return vertex2;	
		}
		
		public double getWeight() {
			return weight;
		}
	}
	
	class weighted_edgesComparator implements Comparator<weighted_edgesItem> {
		// Reverse vertex order
		public int compare(weighted_edgesItem o1, weighted_edgesItem o2) {
			int firstVertex1 = o1.getVertex1();
			int firstVertex2 = o1.getVertex2();
			int secondVertex1 = o2.getVertex1();
			int secondVertex2 = o2.getVertex2();
			if (firstVertex1 < secondVertex1) {
				return 1;
			}
			if (firstVertex1 > secondVertex1) {
				return -1;
			}
			if (firstVertex2 < secondVertex2) {
				return 1;
			}
			if (firstVertex2 > secondVertex2) {
				return -1;
			}
			return 0;
		}
	}
	  
	  class indexIntegerdoubleArrayItem {
		  private int index;
		  private int row_begin;
		  private double[] array;
		  
		  public indexIntegerdoubleArrayItem(int index, int row_begin, double array[]) {
			  this.index = index;
			  this.row_begin = row_begin;
			  this.array = array;
		  }
		  
		  public int getIndex() {
			  return index;
		  }
		  
		  public int getRowBegin() {
			  return row_begin;
		  }
		  
		  public double[] getArray() {
			  return array;
		  }
	  }
	  
	  class indexIntegerdoubleArrayComparator implements Comparator<indexIntegerdoubleArrayItem> {
		  public int compare(indexIntegerdoubleArrayItem o1, indexIntegerdoubleArrayItem o2) {
			  int firstRowBegin = o1.getRowBegin();
			  int secondRowBegin = o2.getRowBegin();
			  if (firstRowBegin < secondRowBegin) {
				  return -1;
			  }
			  if (firstRowBegin > secondRowBegin) {
				  return 1;
			  }
			  return 0;
		  }
	  }
	  
	  private class indexArrayItem {
		  private int index;
		  private double array[];
		  
		  public indexArrayItem(int index, double array[]) {
			  this.index = index;
			  this.array = array;
		  }
		  
		  public int getIndex() {
			  return index;
		  }
		  
		  public double[] getArray() {
			  return array;
		  }
		  
		 
	  }
	  
	  private class indexArrayComparator implements Comparator<indexArrayItem> {
		  public int compare(indexArrayItem o1, indexArrayItem o2) {  
			  int i;
			  double firstArray[] = o1.getArray();
			  double secondArray[] = o2.getArray();
			  if (firstArray.length < secondArray.length) {
				  return -1;
			  }
			  if (firstArray.length > secondArray.length) {
				  return 1;
			  }
			  for (i = 0; i < firstArray.length; i++) {
				  if (firstArray[i] < secondArray[i]) {
					  return -1;
				  }
				  if (firstArray[i] > secondArray[i]) {
					  return 1;
				  }
			  }
			  return 0;
		  }
	  }
	  
	  private class indexArrayArrayComparator implements Comparator<indexArrayArrayItem> {
		  public int compare(indexArrayArrayItem o1, indexArrayArrayItem o2) {
			  int i;
			  double firstArray1[] = o1.getArray1();
			  double firstArray2[] = o1.getArray2();
			  double secondArray1[] = o2.getArray1();
			  double secondArray2[] = o2.getArray2();
			  if (firstArray1.length < secondArray1.length) {
				  return -1;
			  }
			  if (firstArray1.length > secondArray1.length) {
				  return 1;
			  }
			  if (firstArray2.length < secondArray2.length) {
				  return -1;
			  }
			  if (firstArray2.length > secondArray2.length) {
				  return 1;
			  }
			  for (i = 0; i < firstArray1.length; i++) {
				  if (firstArray1[i] < secondArray1[i]) {
					  return -1;
				  }
				  if (firstArray1[i] > secondArray1[i]) {
					  return 1;
				  }
			  }
			  for (i = 0; i < firstArray2.length; i++) {
				  if (firstArray2[i] < secondArray2[i]) {
					  return -1;
				  }
				  if (firstArray2[i] > secondArray2[i]) {
					  return 1;
				  }
			  }
			  return 0;
		  }
	  }
	  
	  private class indexArrayArrayItem {
		  private int index;
		  private double array1[];
		  private double array2[];
		  
		  public indexArrayArrayItem(int index, double array1[], double array2[]) {
			  this.index = index;
			  this.array1 = array1;
			  this.array2 = array2;
		  }
		  
		  public int getIndex() {
			  return index;
		  }
		  
		  public double[] getArray1() {
			  return array1;
		  }
		  
		  public double[] getArray2() {
			  return array2;
		  }
	  }
}