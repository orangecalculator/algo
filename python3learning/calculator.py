#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jan  1 20:20:10 2019

@author: orangecal
"""

class FourCal:
    def __init__(self,a,b):
        self.a=a
        self.b=b
    def setdata(self,a,b):
        self.a=a
        self.b=b
    def add(self):
        return self.a+self.b
    def sub(self):
        return self.a-self.b
    def mul(self):
        return self.a*self.b
    def div(self):
        return self.a/self.b

class MoreFourCal(FourCal):
    def pow(self):
        return self.a ** self.b
    def div(self):
        if self.b==0:
            return 0
        else:
            return self.a/self.b

cc=MoreFourCal(4,2)
print(cc.add())
print(cc.sub())
print(cc.mul())
print(cc.div())
print(cc.pow())

d=MoreFourCal(4,0)
print(d.add())
print(d.sub())
print(d.mul())
print(d.div())
print(d.pow())

class Calculator:
    def __init__(self):
        self.value = 0

    def add(self, val):
        self.value += val

class UpgradeCalculator(Calculator):
    def minus(self, val):
        self.value -= val

class MaxLimitCalculator(UpgradeCalculator):
    def add(self,val):
        self.value+=val
        if(self.value>100):
            self.value=100