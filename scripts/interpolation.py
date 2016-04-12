import numpy as np
regime_dic = {}
regime_dic['NCEP'] = {}
regime_dic['NCEP']['regime 1'] = np.ones([20,30]) * np.random.normal(size=[20,30])
regime_dic['NCEP']['regime 2'] = np.ones([20,30]) * np.random.normal(size=[20,30])
regime_dic['NCEP']['regime 3'] = np.ones([20,30]) * np.random.normal(size=[20,30])
regime_dic['NCEP']['regime 4'] = np.ones([20,30]) * np.random.normal(size=[20,30])

regime_dic['Model_1']= {}
regime_dic['Model_1']['regime 1'] = np.ones([15,20]) * np.random.normal(size=[15,20])
regime_dic['Model_1']['regime 2'] = np.ones([15,20]) * np.random.normal(size=[15,20])
regime_dic['Model_1']['regime 3'] = np.ones([15,20]) * np.random.normal(size=[15,20])
regime_dic['Model_1']['regime 4'] = np.ones([15,20]) * np.random.normal(size=[15,20])

wr.get_NCEPmatrix(regime_dic)

from scipy.signal import correlate2d

scipy.signal.correlate2d(in1, in2, mode='full', boundary='fill', fillvalue=0)


from scipy import signal
from scipy import misc
face = misc.face(gray=True)
scharr = np.array([[ -3-3j, 0-10j,  +3 -3j],
                   [-10+0j, 0+ 0j, +10 +0j],
                   [ -3+3j, 0+10j,  +3 +3j]]) # Gx + j*Gy
grad = signal.convolve2d(face, scharr, boundary='symm', mode='same')

import matplotlib.pyplot as plt
fig, (ax_orig, ax_mag, ax_ang) = plt.subplots(1, 3)
ax_orig.imshow(face, cmap='gray')
ax_orig.set_title('Original')
ax_orig.set_axis_off()
ax_mag.imshow(np.absolute(grad), cmap='gray')
ax_mag.set_title('Gradient magnitude')
ax_mag.set_axis_off()
ax_ang.imshow(np.angle(grad), cmap='hsv') # hsv is cyclic, like angles
ax_ang.set_title('Gradient orientation')
ax_ang.set_axis_off()
fig.show()

from scipy import signal
from scipy import misc
lena = misc.face() - misc.face().mean()
template = np.copy(lena[235:295, 310:370]) # right eye
template -= template.mean()
lena = lena + np.random.randn(*lena.shape) * 50 # add noise
corr = signal.correlate2d(lena, template, boundary='symm', mode='same')
y, x = np.unravel_index(np.argmax(corr), corr.shape) # find the match

import matplotlib.pyplot as plt
fig, (ax_orig, ax_template, ax_corr) = plt.subplots(1, 3)
ax_orig.imshow(lena, cmap='gray')
ax_orig.set_title('Original')
ax_orig.set_axis_off()
ax_template.imshow(template, cmap='gray')
ax_template.set_title('Template')
ax_template.set_axis_off()
ax_corr.imshow(corr, cmap='gray')
ax_corr.set_title('Cross-correlation')
ax_corr.set_axis_off()
ax_orig.plot(x, y, 'ro')
fig.show()


