import numpy as np
import xarray as xr
import re

def distance(val, ref):
    return abs(ref - val)
vectDistance = np.vectorize(distance)

def getClosest(sortedMatrix, column, val):
    while len(sortedMatrix) > 3:
        half = int(len(sortedMatrix) / 2)
        sortedMatrix = sortedMatrix[-half - 1:] if sortedMatrix[half, column] < val else sortedMatrix[: half + 1]
    if len(sortedMatrix) == 1:
        result = sortedMatrix[0].copy()
        result[column] = val
        return result
    else:
        safecopy = sortedMatrix.copy()
        safecopy[:, column] = vectDistance(safecopy[:, column], val)
        minidx = np.argmin(safecopy[:, column])
        safecopy = safecopy[minidx, :].A1
        safecopy[column] = val
        return safecopy

def convert(column, samples, matrix):
    return np.matrix([getClosest(matrix, column, t) for t in samples])

def valueOrEmptySet(k, d):
    return (d[k] if isinstance(d[k], set) else {d[k]}) if k in d else set()

def mergeDicts(d1, d2):
    """
    Creates a new dictionary whose keys are the union of the keys of two
    dictionaries, and whose values are the union of values.

    Parameters
    ----------
    d1: dict
        dictionary whose values are sets
    d2: dict
        dictionary whose values are sets

    Returns
    -------
    dict
        A dict whose keys are the union of the keys of two dictionaries,
    and whose values are the union of values

    """
    res = {}
    for k in d1.keys() | d2.keys():
        res[k] = valueOrEmptySet(k, d1) | valueOrEmptySet(k, d2)
    return res

def extractCoordinates(filename):
    """
    Scans the header of an Alchemist file in search of the variables.

    Parameters
    ----------
    filename : str
        path to the target file
    mergewith : dict
        a dictionary whose dimensions will be merged with the returned one

    Returns
    -------
    dict
        A dictionary whose keys are strings (coordinate name) and values are
        lists (set of variable values)

    """
    with open(filename, 'r') as file:
        regex = re.compile(' (?P<varName>[a-zA-Z]+) = (?P<varValue>[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?),?')
        dataBegin = re.compile('\d')
        for line in file:
            match = regex.findall(line)
            if match:
                return {var : float(value) for var, value in match}
            elif dataBegin.match(line[0]):
                return {}

def extractVariableNames(filename):
    """
    Gets the variable names from the Alchemist data files header.

    Parameters
    ----------
    filename : str
        path to the target file

    Returns
    -------
    list of list
        A matrix with the values of the csv file

    """
    with open(filename, 'r') as file:
        dataBegin = re.compile('\d')
        lastHeaderLine = ''
        for line in file:
            if dataBegin.match(line[0]):
                break
            else:
                lastHeaderLine = line
        if lastHeaderLine:
            regex = re.compile(' (?P<varName>\S+)')
            return regex.findall(lastHeaderLine)
        return []

def openCsv(path):
    """
    Converts an Alchemist export file into a list of lists representing the matrix of values.

    Parameters
    ----------
    path : str
        path to the target file

    Returns
    -------
    list of list
        A matrix with the values of the csv file

    """
    regex = re.compile('\d')
    with open(path, 'r') as file:
        lines = filter(lambda x: regex.match(x[0]), file.readlines())
        return [[float(x) for x in line.split()] for line in lines]

if __name__ == '__main__':
    # CONFIGURE SCRIPT
    directory = 'data'
    pickleOutput = 'data_summary'
    experiments = ['chat_map', 'gossip_map']
    floatPrecision = '{: 0.2f}'
    seedVars = ['random']
    timeSamples = 1000
    minTime = 0
    maxTime = None
    timeColumnName = 'time'
    logarithmicTime = False
    
    # Setup libraries
    np.set_printoptions(formatter={'float': floatPrecision.format})
    # Read the last time the data was processed, reprocess only if new data exists, otherwise just load
    import pickle
    import os
    newestFileTime = max(os.path.getmtime(directory + '/' + file) for file in os.listdir(directory))
    try:
        lastTimeProcessed = pickle.load(open('timeprocessed', 'rb'))
    except:
        lastTimeProcessed = -1
    shouldRecompute = newestFileTime != lastTimeProcessed
    if not shouldRecompute:
        try:
            means = pickle.load(open(pickleOutput + '_mean', 'rb'))
            stdevs = pickle.load(open(pickleOutput + '_std', 'rb'))
        except:
            shouldRecompute = True
    if shouldRecompute:
        timefun = np.logspace if logarithmicTime else np.linspace
        means = {}
        stdevs = {}
        computeMin = minTime is None
        computeMax = maxTime is None
        for experiment in experiments:
            # Collect all files for the experiment of interest
            import fnmatch
            allfiles = filter(lambda file: fnmatch.fnmatch(file, experiment + '_*.txt'), os.listdir(directory))
            allfiles = [directory + '/' + name for name in allfiles]
            allfiles.sort()
            # From the file name, extract the independent variables
            dimensions = {}
            for file in allfiles:
                dimensions = mergeDicts(dimensions, extractCoordinates(file))
            dimensions = {k: sorted(v) for k, v in dimensions.items()}
            # Add time to the independent variables
            dimensions[timeColumnName] = range(0, timeSamples)
            # Compute the matrix shape
            shape = tuple(len(v) for k, v in dimensions.items())
            # Prepare the Dataset
            dataset = xr.Dataset()
            for k, v in dimensions.items():
                dataset.coords[k] = v
            varNames = extractVariableNames(allfiles[0])
            for v in varNames:
                if v != timeColumnName:
                    novals = np.ndarray(shape)
                    novals.fill(float('nan'))
                    dataset[v] = (dimensions.keys(), novals)
            # Compute maximum and minimum time, create the resample
            timeColumn = varNames.index(timeColumnName)
            allData = { file: np.matrix(openCsv(file)) for file in allfiles }
            if computeMax:
                maxTime = float('-inf')
                for data in allData.values():
                    maxTime = max(maxTime, data[-1, timeColumn])
            if computeMin:
                minTime = float('inf')
                for data in allData.values():
                    minTime = min(minTime, data[0, timeColumn])
            timeline = timefun(minTime, maxTime, timeSamples)
            # Resample
            for file in allData:
                allData[file] = convert(timeColumn, timeline, allData[file])
            # Populate the dataset
            for file, data in allData.items():
                dataset[timeColumnName] = timeline
                for idx, v in enumerate(varNames):
                    if v != timeColumnName:
                        darray = dataset[v]
                        experimentVars = extractCoordinates(file)
                        darray.loc[experimentVars] = data[:, idx].A1
            # Fold the dataset along the seed variables, producing the mean and stdev datasets
            means[experiment] = dataset.mean(seedVars)
            stdevs[experiment] = dataset.std(seedVars)
        # Save the datasets
        pickle.dump(means, open(pickleOutput + '_mean', 'wb'), protocol=-1)
        pickle.dump(stdevs, open(pickleOutput + '_std', 'wb'), protocol=-1)
        pickle.dump(newestFileTime, open('timeprocessed', 'wb'))





    # Prepare the charting system
    import matplotlib
    import matplotlib.pyplot as plt
    import matplotlib.cm as cmx
    figure_size=(6, 3)
    
    def timeplot(finalizer, **kwargs):
        fig = plt.figure(figsize = figure_size)
        ax = fig.add_subplot(1, 1, 1)
        if 'title' in kwargs:
            ax.set_title(kwargs['title'])
        if 'maxTime' in kwargs:
            ax.set_xlim(xmax=kwargs['maxTime'])
        if 'minTime' in kwargs:
            ax.set_xlim(xmin=kwargs['minTime'])
        if 'maxy' in kwargs:
            ax.set_ylim(ymax=kwargs['maxy'])
        if 'miny' in kwargs:
            ax.set_ylim(ymin=kwargs['miny'])
        if 'ylabel' in kwargs:
            ax.set_ylabel(kwargs['ylabel'])
        if 'xlabel' in kwargs:
            ax.set_xlabel(kwargs['xlabel'])
        for (label, data) in kwargs['data'].items():
            ax.plot(kwargs['time'], data, label=label)
        fig.tight_layout()
        finalizer(ax)
        fig.savefig(kwargs['title'] + '.pdf')
        return ax

    chatmean = means['chat_map']
    chaterr = stdevs['chat_map']
    gossipmean = means['gossip_map']
    gossiperr = stdevs['gossip_map']
    time = means['chat_map']['time']

#    Chat accuracy
    spawnmean =  chatmean['msg_received_spawn[Sum]'] / chatmean['msg_sent_spawn[Sum]']
    classicmean = chatmean['msg_received_nospawn[Sum]'] / chatmean['msg_sent_nospawn[Sum]']
    accuracy = timeplot(
        lambda ax: ax.legend(),
        title = 'Accuracy',
        ylabel = 'Probability of successful delivery',
        xlabel = 'Time (s)',
        experiment = 'chat_map',
        time = time,
        maxTime = 300,
        data = {
            'Spawn chat': spawnmean,
            'Flood chat': classicmean
        }
    )
    
#    spawnerr = np.append(np.array(float('NaN')), np.diff(chaterr['msg_sent_spawn[Sum]'] + chaterr['msg_received_spawn[Sum]']))
#    accuracy.plot(spawnmean + spawnerr)
    
#    Chat bandwidth
    def finalize(ax):
        ax.set_yscale('log')
        ax.legend()
    band = timeplot(
        finalize,
        title = 'Bandwidth consumption',
        ylabel = 'Messages per second per node',
        xlabel = 'Time (s)',
        experiment = 'chat_map',
        time = time,
        maxTime = 300,
        data = {
            'Spawn chat': np.append(np.diff(chatmean['bandwidth_spawn[Mean]']), float('nan')),
            'Flood chat': np.append(np.diff(chatmean['bandwidth_nospawn[Mean]']), float('nan'))
        }
    )

    bandsum = timeplot(
        finalize,
        title = 'Total bandwith used',
        ylabel = 'Payloads sent',
        xlabel = 'Time (s)',
        experiment = 'chat_map',
        time = time,
        maxTime = 300,
        maxy = 5e8,
        miny = 1,
        data = {
            'Spawn chat': chatmean['bandwidth_spawn[Sum]'],
            'Flood chat': chatmean['bandwidth_nospawn[Sum]']
        }
    )

#    Gossip
    time = means['gossip_map']['time'] / 60
    gossip = timeplot(
        lambda ax: ax.legend(ncol=1),
        title = 'Values',
        ylabel = 'Network value',
        xlabel = 'Time (min)',
        time = time,
        maxTime = 60,
        data = {
            'Naive': gossipmean['gossip_naive_val[Mean]'],
            'Replicated': gossipmean['gossip_replicated_val[Mean]'],
            'S+C+G': gossipmean['gossip_gc_val[Mean]'],
#            'Oracle': gossipmean['gossip_opt_val[Mean]'],
            'Truth': gossipmean['gossip_true_val[Mean]'],
        }
    )
    

    oracleerr = timeplot(
        lambda ax: ax.legend(),
        title = 'Accuracy vs oracle',
        ylabel = 'Error (mean square root)',
        xlabel = 'Time (min)',
        time = time,
        maxTime = 60,
        data = {
            'Naive': gossipmean['gossip_naive_err[Mean]'],
            'Replicated': gossipmean['gossip_replicated_err[Mean]'],
            'S+C+G': gossipmean['gossip_gc_err[Mean]'],
        }
    )
    

    trutherr = timeplot(
        lambda ax: ax.legend(ncol=2),
        title = 'Accuracy',
        ylabel = 'Error (mean square root)',
        xlabel = 'Time (min)',
        time = time,
        maxTime = 60,
        data = {
            'Naive': gossipmean['gossip_naive_true_err[Mean]'],
            'Replicated': gossipmean['gossip_replicated_true_err[Mean]'],
            'S+C+G': gossipmean['gossip_gc_true_err[Mean]'],
#            'Oracle': gossipmean['oracle_true_err[Mean]'],
        }
    )

