/*****
** kmeans.c
** - a simple k-means clustering routine
** - returns the cluster labels of the data points in an array
** - here's an example
**   extern int *k_means(double**, int, int, int, double, double**);
**   ...
**   int *c = k_means(data_points, num_points, dim, 20, 1e-4, 0);
**   for (i = 0; i < num_points; i++) {
**      printf("data point %d is in cluster %d\n", i, c[i]);
**   }
**   ...
**   free(c);
** Parameters
** - array of data points (double **data)
** - number of data points (int n)
** - dimension (int m)
** - desired number of clusters (int k)
** - error tolerance (double t)
**   - used as the stopping criterion, i.e. when the sum of
**     squared euclidean distance (standard error for k-means)
**     of an iteration is within the tolerable range from that
**     of the previous iteration, the clusters are considered
**     "stable", and the function returns
**   - a suggested value would be 0.0001
** - output address for the final centroids (double **centroids)
**   - user must make sure the memory is properly allocated, or
**     pass the null pointer if not interested in the centroids
** References
** - J. MacQueen, "Some methods for classification and analysis
**   of multivariate observations", Fifth Berkeley Symposium on
**   Math Statistics and Probability, 281-297, 1967.
** - I.S. Dhillon and D.S. Modha, "A data-clustering algorithm
**   on distributed memory multiprocessors",
**   Large-Scale Parallel Data Mining, 245-260, 1999.
** Notes
** - this function is provided as is with no warranty.
** - the author is not responsible for any damage caused
**   either directly or indirectly by using this function.
** - anybody is free to do whatever he/she wants with this
**   function as long as this header section is preserved.
** Created on 2005-04-12 by
** - Roger Zhang (rogerz@cs.dal.ca)
** Modifications
** -
** Last compiled under Linux with gcc-3
*/

#include <stdlib.h>
#include <assert.h>
#include <float.h>
#include <math.h>

int *k_means(double **data, int n, int m, int k, double t, double **centroids)
{
   /* output cluster label for each data point */
   int *labels = (int*)calloc(n, sizeof(int));

   int h, i, j; /* loop counters, of course :) */
   int *counts = (int*)calloc(k, sizeof(int)); /* size of each cluster */
   double old_error, error = DBL_MAX; /* sum of squared euclidean distance */
   double **c = centroids ? centroids : (double**)calloc(k, sizeof(double*));
   double **c1 = (double**)calloc(k, sizeof(double*)); /* temp centroids */

   assert(data && k > 0 && k <= n && m > 0 && t >= 0); /* for debugging */

   /****
   ** initialization */

   for (h = i = 0; i < k; h += n / k, i++) {
      c1[i] = (double*)calloc(m, sizeof(double));
      if (!centroids) {
         c[i] = (double*)calloc(m, sizeof(double));
      }
      /* pick k points as initial centroids */
      for (j = m; j-- > 0; c[i][j] = data[h][j]);
   }

   /****
   ** main loop */

   do {
      /* save error from last step */
      old_error = error, error = 0;

      /* clear old counts and temp centroids */
      for (i = 0; i < k; counts[i++] = 0) {
         for (j = 0; j < m; c1[i][j++] = 0);
      }

      for (h = 0; h < n; h++) {
         /* identify the closest cluster */
         double min_distance = DBL_MAX;
         for (i = 0; i < k; i++) {
            double distance = 0;
            for (j = m; j-- > 0; distance += pow(data[h][j] - c[i][j], 2));
            if (distance < min_distance) {
               labels[h] = i;
               min_distance = distance;
            }
         }
         /* update size and temp centroid of the destination cluster */
         for (j = m; j-- > 0; c1[labels[h]][j] += data[h][j]);
         counts[labels[h]]++;
         /* update standard error */
         error += min_distance;
      }

      for (i = 0; i < k; i++) { /* update all centroids */
         for (j = 0; j < m; j++) {
            c[i][j] = counts[i] ? c1[i][j] / counts[i] : c1[i][j];
         }
      }

   } while (fabs(error - old_error) > t);

   /****
   ** housekeeping */

   for (i = 0; i < k; i++) {
      if (!centroids) {
         free(c[i]);
      }
      free(c1[i]);
   }

   if (!centroids) {
      free(c);
   }
   free(c1);

   free(counts);

   return labels;
}
