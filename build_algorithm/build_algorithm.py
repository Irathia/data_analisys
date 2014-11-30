import re
import os
import argparse
from matplotlib import colors
from matplotlib import axes
from matplotlib import pyplot
import pylab

def build_algorithm(x_matrix, K = 3):
    distance_arr = dict()
    for key in x_matrix.keys():
        distance_arr[key] = 0
        for item in x_matrix[key]:
            distance_arr[key] += item
        distance_arr[key] = round(distance_arr[key],1)

    if K < 1 or K > len(x_matrix):
        exit('Error: incorrect K = %s! The number of objects is %s...' % (K,len(x_matrix)))

    centroids = dict()
    min_elem = ['', -1]
    for key in distance_arr.keys():
        if min_elem[1] == -1:
            min_elem[0] = key
            min_elem[1] = distance_arr[key]
        elif min_elem[1] > distance_arr[key]:
            min_elem[0] = key
            min_elem[1] = distance_arr[key]
    zerro_pos = 0
    for i in range(0, len(x_matrix[min_elem[0]])):
        if x_matrix[min_elem[0]][i] == 0:
            zerro_pos = i
            break
    centroids[min_elem[0]] =  zerro_pos

    while(len(centroids) < K):
        Eclusters = dict()
        for key in x_matrix.keys():
            if key in centroids:
                continue
            Eclusters[key] = 0

            zerro_pos = 0
            for i in range(0, len(x_matrix[key])):
                if x_matrix[key][i] == 0:
                    zerro_pos = i
                    break

            min_centroid = ['', -1]
            for name_centroid in centroids.keys():
                if min_centroid[1] == -1 or min_centroid[1] > x_matrix[key][centroids[name_centroid]]:
                    min_centroid[0] = name_centroid
                    min_centroid[1] = x_matrix[key][centroids[name_centroid]]
            min_centroid[1] = centroids[min_centroid[0]]

            for key_other in x_matrix.keys():
                if key_other in centroids or key_other == key:
                    continue
                if x_matrix[key_other][min_centroid[1]] > x_matrix[key_other][zerro_pos]:
                    Eclusters[key] += x_matrix[key][min_centroid[1]] - x_matrix[key_other][zerro_pos]
        max_elem = ['', -1]
        for key in Eclusters.keys():
            if max_elem[1] == -1 or max_elem[1] < Eclusters[key]:
                max_elem[0] = key
                max_elem[1] = Eclusters[key]
        zerro_pos = 0
        for i in range(0, len(x_matrix[max_elem[0]])):
            if x_matrix[max_elem[0]][i] == 0:
                zerro_pos = i
                break
        centroids[max_elem[0]] =  zerro_pos

    clasters = dict()
    for key in sorted(centroids.keys()):
        clasters[key] = []
    for key in x_matrix.keys():
        min_dist = ['', -1]
        for name_centroid in sorted(centroids.keys()):
            if min_dist[1] == -1 or min_dist[1] > x_matrix[key][centroids[name_centroid]]:
                min_dist[1] = x_matrix[key][centroids[name_centroid]]
                min_dist[0] = name_centroid

        clasters[min_dist[0]].append(key)
    #return_clasters = []
    #for key in sorted(clasters.keys()):
    #    return_clasters.append(clasters[key])
    return clasters

def error_args(file_k, clasters, type):
    factors_name = dict()
    with open(file_k, 'r') as f:
        count = 0
        for line in f:
            if count == 0:
                count += 1
                continue
            line = line.rstrip()
            line = line.lstrip()
            factors = line.split(',')
            factors[0] = 'V' + re.compile('\"').sub('', factors[0])
            factors[-1] = re.compile('\"').sub('', factors[-1])
            if factors[-1] not in factors_name:
                factors_name[factors[-1]] = []
            factors_name[factors[-1]].append(factors[0])
    check_claster = dict()
    for key in clasters.keys():
        for key_factor in factors_name.keys():
            if key in factors_name[key_factor]:
                check_claster[key_factor] = clasters[key]

    all_elements = 0
    error_elements = 0
    for key in check_claster.keys():
        for element in check_claster[key]:
            all_elements += 1
            if element not in factors_name[key]:
                error_elements += 1
    return error_elements / all_elements


if __name__=='__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--csv', dest='csv')
    parser.add_argument('--check_error', dest='check_error')
    parser.add_argument('--check_test', dest='check_test')

    parser.add_argument('--K', dest='k')
    args = parser.parse_args()

    if not args.csv:
        exit('Csv file is not set!')
    if not args.k:
        args.k = 3
    else:
        args.k = int(args.k)

    if not os.path.exists(args.csv):
        exit('Csv file is not exist!')

    with open(args.csv, 'r') as f:
        lines = f.readlines();

    x_matrix = dict()
    names_arr = []
    for i in range(0, len(lines)):
        lines[i] = lines[i].lstrip()
        lines[i] = lines[i].rstrip()
        values = lines[i].split(',')
        if i == 0:
            try:
                float(values[0])
                for j in range(0, len(values)):
                    names_arr.append(j)
            except:
                if values[0] == "\"\"" or values[0] == "":
                    for j in range(1, len(values)):
                        values[j] = re.compile("\"").sub('', values[j])
                        names_arr.append(values[j])
                else:
                    for value in values:
                        value = re.compile("\"").sub('', value)
                        names_arr.append(value)
                continue
        start_from = 0
        try:
            float(values[0])
        except:
            start_from = 1
        for j in range(start_from, len(values)):
            if names_arr[j - start_from] not in x_matrix:
                x_matrix[names_arr[j - start_from]] = []
            x_matrix[names_arr[j - start_from]].append(float(values[j]))

    clasters = build_algorithm(x_matrix, args.k)
    print(clasters)

    if args.check_error:
        print(error_args(args.check_error, clasters, args.check_test))

    zerro_point = ['', -1]
    for key in sorted(x_matrix.keys()):
        zerro_point[0] = key
        for i in range(0, len(x_matrix[key])):
            if x_matrix[key][i] == 0:
                zerro_point[1] = i
                break
        break


    colors_points = []
    for i in range(0, args.k):
        j = 0
        for key in colors.cnames.keys():
            if j == i:
                colors_points.append(key)
                break
            j += 1

    #for i in range(0, len(clasters)):
    #    x = []
    #    for obj_name in clasters[i]:
    #        x.append(x_matrix[obj_name][zerro_point[1]])
    #    pylab.scatter(x,x,color=colors_points[i])

    #pylab.show()