### stock problem example for ffnet ###

# Data and description for this example is borrowed from:
# http://www.scientific-consultants.com/nnbd.html
#
#
# Training data consists of Black-Scholes option prices
# for volatility levels running from 20 percent to 200 percent,
# for time remaining running from 5 to 15 days, and for strike price
# running from 70 dollars to 130 dollars. The stock price was set to
# 100 dollars and the interest rate to 0 percent when generating
# the data.
#
# The data is "difficult" in that (for a neural network to
# practically emulate Black-Scholes) a very tight fit is required.
# The R-squared should be at least 0.999999 or better, and the largest
# absolute error must be less than 0.05 dollars (the price increment
# for most options) or, better yet, less than 0.01 dollars.
#
#
# So let's try.
# Attention: training might be a long process since we train a big network.

from ffnet import ffnet, mlgraph, readdata
from numpy import array
from random import shuffle
from math import floor, ceil

# Generate standard layered network architecture and create network
conec = mlgraph((3,22,12,1))
net = ffnet(conec)

# Read training data omitting first column and first line
print "READING DATA..."
data = readdata( 'data/black-scholes.dat',
                 usecols  = (1, 2, 3, 4),
                 skiprows =  1)

# split data into train and test
shuffle(data)
trainData = data[0:floor(len(data) * 0.75)]
testData = data[ceil(len(data) * 0.75):(len(data) - 1)]
trainInput =  trainData[:, :3] #first 3 columns
trainTarget = trainData[:, -1] #last column
testInput =  testData[:, :3] #first 3 columns
testTarget = testData[:, -1] #last column

print "TRAINING NETWORK..."
import sys; sys.stdout.flush() #Just to ensure dislpaing the above messages here
net.train_tnc(trainInput, trainTarget, maxfun = 5000, messages=1)

# Test network
print
print "TESTING NETWORK..."
output, regression = net.test(testInput, testTarget, iprint = 0)
Rsquared = regression[0][2]
maxerr = abs( array(output).reshape( len(output) ) - array(testTarget) ).max()
print "R-squared:           %s  (should be >= 0.999999)" %str(Rsquared)
print "max. absolute error: %s  (should be <= 0.05)" %str(maxerr)
print
print "Is ffnet ready for a stock?"

#####################################
# Make plot if matplotlib is avialble
try:
    from pylab import *
    plot( testTarget, 'b--' )
    plot( output, 'k-' )
    legend(('target', 'output'))
    xlabel('pattern'); ylabel('price')
    title('Outputs vs. target of trained network.')
    grid(True)
    show()
except ImportError, e:
    print "Cannot make plots. For plotting install matplotlib.\n%s" % e
