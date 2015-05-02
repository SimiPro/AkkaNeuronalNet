package simi.net

import scala.collection.mutable

/**
 * Created by Simon on 19.04.2015.
 */
class NetBuilder(var inputLayerUnits:Int = 2, var outputLayerUnits:Int = 1, var defaultHiddenLayers:Boolean = true,
                  var inputlayername:String = "inputlayer",  var outputlayername:String = "outputlayer") {
  var hiddenLayers = mutable.MutableList[HiddenLayerBuilder]()
  createHiddenLayers()


  def createHiddenLayers() = {
    if (defaultHiddenLayers) addHiddenLayer()
  }



  def addHiddenLayer(hiddenLayerUnits:Int = 1): NetBuilder = {
    hiddenLayers += new HiddenLayerBuilder(hiddenLayerUnits, 0)
    this
  }

  class HiddenLayerBuilder(var hiddenLayerUnits:Int = 1, index:Int) {
     var name:String  = "hiddenlayer" + index
  }

}
