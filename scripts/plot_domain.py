"""
This example demonstrates the way a box is warped when it is defined
in a rotated pole coordinate system.

Try changing the ``box_top`` to ``44``, ``46`` and ``75`` to see the effect
that including the pole in the polygon has.

"""

import matplotlib.pyplot as plt
import cartopy.crs as ccrs


def main():
    rotated_pole = ccrs.RotatedPole(pole_latitude=45, pole_longitude=180)

    box_top = 45
    x, y = [-44, -44, 45, 45, -44], [-45, box_top, box_top, -45, -45]

    ax = plt.subplot(211, projection=rotated_pole)
    ax.stock_img()
    ax.coastlines()
    ax.plot(x, y, marker='o', transform=rotated_pole)
    ax.fill(x, y, color='coral', transform=rotated_pole, alpha=0.4)
    ax.gridlines()

    ax = plt.subplot(212, projection=ccrs.PlateCarree())
    ax.stock_img()
    ax.coastlines()
    ax.plot(x, y, marker='o', transform=rotated_pole)
    ax.fill(x, y, transform=rotated_pole, color='coral', alpha=0.4)
    ax.gridlines()

    plt.show()


if __name__ == '__main__':
    main()
#
#
#
# import matplotlib.pyplot as plt
# import matplotlib.patches as mpatches
#
# import cartopy.crs as ccrs
#
# desired_projections = [ccrs.PlateCarree(),
#                        ccrs.RotatedPole(pole_latitude=45, pole_longitude=180)]
# for plot_num, desired_proj in enumerate(desired_projections):
#
#     ax = plt.subplot(2, 1, plot_num + 1, projection=desired_proj)
#
#     ax.set_global()
#
#     ax.add_patch(mpatches.Rectangle(xy=[-70, -45], width=90, height=90,
#                                     facecolor='blue',
#                                     alpha=0.2,
#                                     transform=ccrs.PlateCarree())
#                  )
#
#     ax.add_patch(mpatches.Rectangle(xy=[70, -45], width=90, height=90,
#                                     facecolor='red',
#                                     alpha=0.2,
#                                     transform=ccrs.Geodetic())
#                  )
#
#     ax.gridlines()
#     ax.coastlines()
#
# plt.show()
