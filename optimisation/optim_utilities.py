#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
A collection of utilities for testing optimizers

@author: Rodolphe Le Riche, Brian Dédji Whannou
"""
from cProfile import label

import numpy as np
import matplotlib.pyplot as plt
from typing import Callable, List
import warnings


#################################


def record_hist(
    rec: dict, f: float, x: np.ndarray, time: int, key_f: str, key_x: str, key_time: str
) -> dict:
    """generic function to update history records"""
    if key_f not in rec:
        # first recording, create fields
        rec[key_f] = [f]
        rec[key_time] = [time]
        rec[key_x] = x
    else:
        rec[key_f].append(f)
        rec[key_time].append(time)
        rec[key_x] = np.vstack([rec[key_x], x])

    return rec


#################################


def record_best(
    rec: dict, fbest: float, xbest: np.ndarray, time: int, printlevel: int
) -> dict:
    """create and update records of best so far"""
    rec["x_best"] = xbest
    rec["f_best"] = fbest
    if printlevel > 0:
        rec = record_hist(
            rec=rec,
            f=fbest,
            x=xbest,
            time=time,
            key_f="hist_f_best",
            key_x="hist_x_best",
            key_time="hist_time_best",
        )

    return rec


def record_any(rec: dict, f: float, x: np.ndarray, time: int, printlevel: int) -> dict:
    """create and update record of any point during the search"""
    rec["time_used"] = time
    if printlevel > 1:
        rec = record_hist(
            rec=rec,
            f=f,
            x=x,
            time=time,
            key_f="hist_f",
            key_x="hist_x",
            key_time="hist_time",
        )

    return rec


#################################


def print_rec(
    res: dict,
    fun: Callable,
    dim: int,
    LB: np.array,
    UB: np.array,
    printlevel: int,
    logscale=False,
):
    print(
        f'search stopped after {res["time_used"]} evaluations of f because of {res["stop_condition"]}'
    )
    print("best objective function =", res["f_best"])
    print("best x =", res["x_best"])
    if printlevel > 0:
        fig1, ax1 = plt.subplots()
        if logscale:
            if min(res["hist_f_best"])<= 0:
                warnings.warn("cannot log scale y with negative value, removing the logscale option")
                logscale = False
            else:
                plt.yscale("log")
                # plt.xscale("log")
        ax1.plot(res["hist_time_best"], res["hist_f_best"],label="best")
        ax1.set_xlabel("no. calls to f")
        ax1.set_ylabel("f")
        if printlevel > 1:
            ax1.plot(res["hist_time"], res["hist_f"],label="all points")
        plt.legend()
        if dim == 2:
            # 2D contour plot
            # start drawing the function (necessarily dim==2)
            no_grid = 100
            n_best_pts = len(res["hist_f_best"])
            x1 = np.linspace(start=LB[0], stop=UB[0], num=no_grid)
            x2 = np.linspace(start=LB[1], stop=UB[1], num=no_grid)
            x, y = np.meshgrid(x1, x2)
            xy = np.array([x, y])
            z = np.apply_along_axis(fun, 0, xy)
            fig2, ax2 = plt.subplots()
            # fquant = np.quantile(a=res["hist_f_best"],q=[0,0.05,0.1,0.2,0.5,0.7,1])
            fmin = min(res["hist_f_best"])
            fmax = max(res["hist_f_best"])
            if ((fmax-fmin)>1):
                fquant = fmin + (np.linspace(start=0, stop=1, num=10) ** 2) * (fmax - fmin)
            else:
                fquant = fmin + (np.linspace(start=0, stop=1, num=10) ** 2) * (np.max(z) - np.min(z))

            CS = ax2.contour(x, y, z, levels=fquant)
            ax2.clabel(CS, inline=True, fontsize=10)
            # add history of best points onto it
            if printlevel > 1:
                ax2.plot(res["hist_x"][:, 0], res["hist_x"][:, 1], "ob", markersize=3)
            ax2.plot(
                np.atleast_2d(res["hist_x_best"])[:, 0], np.atleast_2d(res["hist_x_best"])[:, 1], "or", markersize=4
            )
            for i in np.linspace(0, n_best_pts-1, 10).astype(int):
                ax2.text(np.atleast_2d(res["hist_x_best"])[i, 0], np.atleast_2d(res["hist_x_best"])[i, 1], str(i), fontsize=10, ha='right')
    plt.show()