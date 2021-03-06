#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int *k_means(double **data, int n, int m, int k, double t, double **centroids);

// Generate a series of points with Python:
// $ python
// >>> from random import random
// >>> for i in xrange(0, 100):
// ...   print "%f, %f," % (random(), random())
// ...

// Our points are 2D.
#define DIM 2
double _points[] = {
    0.261532, 0.994767,
    0.189796, 0.395445,
    0.464336, 0.942981,
    0.127519, 0.783683,
    0.198156, 0.455893,
    0.325976, 0.049068,
    0.513962, 0.609684,
    0.420884, 0.376269,
    0.752493, 0.645708,
    0.323299, 0.986590,
    0.734552, 0.945726,
    0.085129, 0.009066,
    0.987835, 0.856847,
    0.311116, 0.996606,
    0.259005, 0.479349,
    0.360448, 0.177048,
    0.274590, 0.261456,
    0.407926, 0.714242,
    0.699075, 0.550894,
    0.218721, 0.562646,
    0.202722, 0.212413,
    0.002114, 0.613670,
    0.771235, 0.462637,
    0.568568, 0.396864,
    0.708317, 0.949553,
    0.489927, 0.236356,
    0.346089, 0.048343,
    0.279326, 0.158637,
    0.779576, 0.167133,
    0.022323, 0.671196,
    0.888422, 0.647640,
    0.318304, 0.369705,
    0.309280, 0.518526,
    0.388672, 0.177055,
    0.303647, 0.066516,
    0.769742, 0.126793,
    0.298411, 0.133848,
    0.508084, 0.260788,
    0.410745, 0.810696,
    0.970292, 0.926078,
    0.906445, 0.356729,
    0.167466, 0.564938,
    0.071486, 0.036545,
    0.844680, 0.586603,
    0.043973, 0.685188,
    0.901410, 0.128322,
    0.332381, 0.542794,
    0.625714, 0.781586,
    0.024332, 0.578820,
    0.685595, 0.020977,
    0.809402, 0.455111,
    0.832874, 0.278773,
    0.427298, 0.097117,
    0.457244, 0.339446,
    0.689877, 0.394847,
    0.120568, 0.278134,
    0.725667, 0.617147,
    0.171494, 0.974869,
    0.254644, 0.409328,
    0.943588, 0.856694,
    0.343202, 0.559121,
    0.844857, 0.426168,
    0.181093, 0.051004,
    0.820017, 0.191583,
    0.281435, 0.540966,
    0.566059, 0.247244,
    0.853245, 0.412100,
    0.399483, 0.667527,
    0.055677, 0.595302,
    0.188973, 0.974128,
    0.450814, 0.097911,
    0.193170, 0.796510,
    0.315190, 0.132410,
    0.668082, 0.710962,
    0.398014, 0.056004,
    0.748009, 0.228848,
    0.695031, 0.601013,
    0.702289, 0.011518,
    0.843734, 0.354048,
    0.996430, 0.181244,
    0.365826, 0.541527,
    0.550336, 0.186203,
    0.184034, 0.911895,
    0.436352, 0.646329,
    0.559331, 0.245987,
    0.179594, 0.440002,
    0.472389, 0.619161,
    0.408341, 0.324531,
    0.664037, 0.502712,
    0.791278, 0.971705,
    0.274334, 0.572940,
    0.505988, 0.405872,
    0.625758, 0.545222,
    0.818839, 0.850024,
    0.949917, 0.555353,
    0.541435, 0.195608,
    0.902344, 0.625000,
    0.199219, 0.273438,
    0.417969, 0.753906,
    0.851562, 0.265625,
};
size_t   npoints = sizeof(_points) / sizeof(_points[0]) / DIM;
double** points;
int*     clusters;
double** centroids;
size_t   nclusters;

int main(int argc, const char *argv[])
{
    // build k_means() compatible memory layout
    points = calloc(npoints, sizeof(double*));
    for (int i = 0; i < npoints; ++i) {
        points[i] = &_points[i * DIM];
    }

    // determine the number of points - this is a fragile heuristic!
    nclusters = sqrt(npoints / 2);

    //
    centroids = calloc(nclusters, sizeof(double*));
    for (int i = 0; i < nclusters; ++i) {
        centroids[i] = calloc(2, sizeof(double));
    }

    // print our data
    //printf("npoints: %ld\n", npoints);  
    //for (int i = 0; i < npoints; ++i) {
    //    printf("  %f %f\n", points[i][0], points[i][1]);
    //}

    // run k_means()
    clusters = k_means(points, npoints, DIM, nclusters, 0.0001, centroids);

    // print clusters
    printf("npoints: %ld\n", npoints);
    printf("nclusters: %ld\n", nclusters);
    // x, y, cluster, centroid x, centroid y
    printf("  %8s %8s %2s %8s %8s\n", "x", "y", "c", "cx", "cy");
    for (int i = 0; i < npoints; ++i) {
        int cluster = clusters[i];
        printf("  %8f %8f %2d %f %f\n", points[i][0], points[i][1], cluster,
                                        centroids[cluster][0],
                                        centroids[cluster][1]);
    }

    // print just the centroids
    printf("\ncentroids:\n");
    for (int i = 0; i < nclusters; ++i) {
        int cluster = clusters[i];
        printf("  %2d %f\n", i, centroids[i][0]);
    }
    printf("\n");
    for (int i = 0; i < nclusters; ++i) {
        printf("  %2d %f\n", i, centroids[i][1]);
    }

    // clean up
    if (points)
        free(points);
    if (centroids) {
        for (int i = 0; i < nclusters; ++i) {
            if (centroids[i]) free(centroids[i]);
        }
        free(centroids);
    }

    return 0;
}
