// acude serrinha 
var serra = ee.FeatureCollection('projects/ee-geovaneandradesilvatito/assets/clorofilaserrinha');
 

// add layer to map
Map.addLayer(serra,{},"Serrinha");
// contorno
var empty = ee.Image().byte();
var outline = empty.paint({
  featureCollection: roi,
  color: 1,
  width: 2
});
Map.addLayer(outline, {palette: 'FF0000'}, 'Roi')
//Map.centerObject(serra,10);
                                            
// mascara de nuvem
function maskS2clouds(image) {
  var qa = image.select('QA60');
  // Bits 10 and 11 are clouds and cirrus, respectively.
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;

  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
      .and(qa.bitwiseAnd(cirrusBitMask).eq(0));

  return image.updateMask(mask)
      .divide(10000) //se ligar nesse divide por 10000
      .select('B.*')
      .copyProperties(image, ['system:time_start']);
}

// função dos indice        
function f_index_ (image) {
  
  var NDCI = image.expression(
    '((red_edge - red)/(red_edge + red))', {
    'red_edge': image.select('B5'), 
    'red': image.select('B4')
    }).rename('NDCI')

  var S1 = image.expression(
    '((red_edge - blue)/( blue + red_edge))', {
    'blue': image.select('B2'), 
    'red_edge': image.select('B5'),
    }).rename('S1')  
  
  var S2 = image.expression(
    '((red_edge - blue)/(green))', {
    'blue': image.select('B2'),
    'red_edge': image.select('B5'), 
    'green': image.select('B3')
    }).rename('S2')  
    
  var S3 = image.expression(
    '((red_edge - blue))/(red + blue)', {
    'red_edge': image.select('B5'), 
    'red': image.select('B4'),
    'blue': image.select('B2')
    }).rename('S3') 
    
  var S4 = image.expression(
    '(((red - blue) - (red_edge - red)) / ((red + blue)))', {
    'blue': image.select('B2'),
    'red_edge': image.select('B6'), 
    'red': image.select('B4')
    }).rename('S4')  
    
  var S5 = image.expression(
    '((red - blue) / (green + blue))', {
    'green': image.select('B3'),
    'red_edge': image.select('B6'), 
    'blue': image.select('B2'),
    'red': image.select('B4')
    }).rename('S5') 
    
   var S6 = image.expression(
    '((red_edge - blue)/(red + red_edge))', {
    'blue': image.select('B2'),
    'red_edge': image.select('B5'), 
    'red': image.select('B4')
    }).rename('S6') 
    
    
  var S7 = image.expression(
    '((red_edge - red)/( red_edge))', {
    'red_edge': image.select('B5'), 
    'red': image.select('B4')
    }).rename('S7') 
    
  var S8 = image.expression(
    '((red_edge - blue)/(red))', {
    'blue': image.select('B2'),
    'red_edge': image.select('B5'), 
    'red': image.select('B4')
    }).rename('S8') 
  // modelos teste 
  
   var CI1 = image.expression(
    '((red_edge / blue) - 1)', {
    'red_edge': image.select('B5'),
    'blue': image.select('B2')
    }).rename('CI1') 
 
  var CI2 = image.expression(
    '((red_edge / red) - 1)', {
    'red_edge': image.select('B5'),
    'red': image.select('B4')
    }).rename('CI2')   
   

  var CI3 = image.expression(
  '((red_edge * red) / pow(green, 2))', {
    'red_edge': image.select('B5'),
    'red': image.select('B4'),
    'green': image.select('B3')
  }).rename('CI3'); 
  
  var CI4 = image.expression(
  '((red_edge * red) / pow(blue, 2))', {
    'red_edge': image.select('B5'),
    'red': image.select('B4'),
    'blue': image.select('B2')
  }).rename('CI4'); 
  
  var CI5 = image.expression(
  '((re1 - blue) / blue)', {
    're1': image.select('B5'),   
    'blue': image.select('B2')   
  }).rename('CI5');
  //aquicerto
// modelos teste II
 var G1 = image.expression(
  '((green) / (blue))', {
    'green': image.select('B3'),   
    'blue': image.select('B2')   
  }).rename('G1');
  
  var G2 = image.expression(
    '(red / green)', {
    'green': image.select('B3'),   
    'red': image.select('B4')   
  }).rename('G2');
  
  //modelos IIIIIIIIIIIIIIIIII
 // B8/B4
var R1 = image.expression(
  '(nir / red)', {
    'nir': image.select('B8'),
    'red': image.select('B4')
}).rename('R1');

// B8/B5
var R2 = image.expression(
  '(nir / re1)', {
    'nir': image.select('B8'),
    're1': image.select('B5')
}).rename('R2');

// B8/B6
var R3 = image.expression(
  '(nir / re2)', {
    'nir': image.select('B8'),
    're2': image.select('B6')
}).rename('R3');

// B8/B7
var R4 = image.expression(
  '(nir / re3)', {
    'nir': image.select('B8'),
    're3': image.select('B7')
}).rename('R4');

// B8/B8A
var R5 = image.expression(
  '(nir / nir8a)', {
    'nir': image.select('B8'),
    'nir8a': image.select('B8A')
}).rename('R5');

// B8A/B4
var R6 = image.expression(
  '(nir8a / red)', {
    'nir8a': image.select('B8A'),
    'red': image.select('B4')
}).rename('R6');

// B8A/B5
var R7 = image.expression(
  '(nir8a / re1)', {
    'nir8a': image.select('B8A'),
    're1': image.select('B5')
}).rename('R7');

// B8A/B6
var R8 = image.expression(
  '(nir8a / re2)', {
    'nir8a': image.select('B8A'),
    're2': image.select('B6')
}).rename('R8');

// B8A/B7
var R9 = image.expression(
  '(nir8a / re3)', {
    'nir8a': image.select('B8A'),
    're3': image.select('B7')
}).rename('R9');

// B7/B4
var R10 = image.expression(
  '(re3 / red)', {
    're3': image.select('B7'),
    'red': image.select('B4')
}).rename('R10');

// B7/B5
var R11 = image.expression(
  '(re3 / re1)', {
    're3': image.select('B7'),
    're1': image.select('B5')
}).rename('R11');

// B7/B6
var R12 = image.expression(
  '(re3 / re2)', {
    're3': image.select('B7'),
    're2': image.select('B6')
}).rename('R12');

// B6/B5
var R13 = image.expression(
  '(re2 / re1)', {
    're2': image.select('B6'),
    're1': image.select('B5')
}).rename('R13');

// B6/B4
var R14 = image.expression(
  '(re2 / red)', {
    're2': image.select('B6'),
    'red': image.select('B4')
}).rename('R14');

// B5/B4
var R15 = image.expression(
  '(re1 / red)', {
    're1': image.select('B5'),
    'red': image.select('B4')
}).rename('R15');
 
///////////////////////////////////aqui daquiiiiiiiiii
var N1 = image.expression(
  '((nir - nir8a) / (nir + nir8a))', {
    'nir': image.select('B8'),
    'nir8a': image.select('B8A')
}).rename('N1');

var N2 = image.expression(
  '((nir8a - red) / (nir8a + red))', {
    'nir8a': image.select('B8A'),
    'red': image.select('B4')
}).rename('N2');

var N3 = image.expression(
  '((nir8a - re1) / (nir8a + re1))', {
    'nir8a': image.select('B8A'),
    're1': image.select('B5')
}).rename('N3');

var N4 = image.expression(
  '((nir8a - re2) / (nir8a + re2))', {
    'nir8a': image.select('B8A'),
    're2': image.select('B6')
}).rename('N4');

var N5 = image.expression(
  '((nir8a - re3) / (nir8a + re3))', {
    'nir8a': image.select('B8A'),
    're3': image.select('B7')
}).rename('N5');

var N6 = image.expression(
  '((re3 - red) / (re3 + red))', {
    're3': image.select('B7'),
    'red': image.select('B4')
}).rename('N6');

var N7 = image.expression( /////////////
  '((re3 - re1) / (re3 + re1))', {
    're3': image.select('B7'),
    're1': image.select('B5')
}).rename('N7');
var N8 = image.expression(
  '((re3 - re2) / (re3 + re2))', {
    're3': image.select('B7'),
    're2': image.select('B6')
}).rename('N8');
var N9 = image.expression(
  '((re2 - re1) / (re2 + re1))', {
    're2': image.select('B6'),
    're1': image.select('B5')
}).rename('N9');

var N10 = image.expression(
  '((re2 - red) / (re2 + red))', {
    're2': image.select('B6'),
    'red': image.select('B4')
}).rename('N10');

var N11 = image.expression(
  '((re1 - red) / (re1 + red))', {
    're1': image.select('B5'),
    'red': image.select('B4')
}).rename('N11');


var IN1 = image.expression(
  '((1 / red) - (1 / re1)) * re2', {
    'red': image.select('B4'),
    're1': image.select('B5'),
    're2': image.select('B6')
}).rename('IN1');

var IN2 = image.expression(
  '((1 / red) - (1 / re1)) * nir', {
    'red': image.select('B4'),
    're1': image.select('B5'),
    'nir': image.select('B8')
}).rename('IN2');

var IN3= image.expression(
  '((1 / red) - (1 / re1)) * re3', {
    'red': image.select('B4'),
    're1': image.select('B5'),
    're3': image.select('B7')
}).rename('IN3');
var IN4 = image.expression(
  '((1 / red) - (1 / re1)) * nir', {
    'red': image.select('B4'),
    're1': image.select('B5'),
    'nir': image.select('B8')
}).rename('IN4');
var IN5 = image.expression(
  '((1 / red) - (1 / re1)) * nir8a', {
    'red': image.select('B4'),
    're1': image.select('B5'),
    'nir8a': image.select('B8A')
}).rename('IN5');

  return image.addBands([
NDCI, S1, S2, S3, S4, S5, S6, S7, S8, CI1, CI2, CI3, CI4, CI5,G1, G2,R1, R2, R3, R4, R5,
R6, R7, R8, R9,R10, R11, R12,R13, R14, R15,N1, N2, N3, N4, N5,
N6, N7, N8,N9, N10, N11,IN1, IN2, IN3, IN4, IN5
   ].map(function(band) {
  return band.toFloat();
}));
 }
// melhorar as bordas
function addNDWI(image) {
  var ndwi = image.normalizedDifference(['B3', 'B8']).rename('NDWI'); // pode ser o MNDWI
  return image.addBands(ndwi);
}

function maskWater(image) {
  var waterMask = image.select('NDWI').gt(0.15);
  return image.updateMask(waterMask);
}

function removeEdges(image) {
  var water = image.select('NDWI').gt(0.15);
  
  var deepWater = water
      .focal_min(1.5, 'plus');  // remove 1 pixel de borda (10–20m)

  return image.updateMask(deepWater);
}


  
// that occurred after 2022-01-25).
var sentinel = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
                  .filterBounds(roi)
                  .filterDate('2025-12-10', '2025-12-20')
                  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 1))
                  .map(maskS2clouds)
                  .map(f_index_)
                  .map(addNDWI)
                  .map(removeEdges)
                  .map(function(image) { return image.toFloat(); }) // transformar em float
                  ;

// contagemm de imagem: 
print("Quantas imagens tenho",sentinel.size())
print("Qual é o nome das bandas",sentinel.first().bandNames())

//Atribuir propriedade de data na coleção
var collection = sentinel.map(function(image){
  return image.clip(roi)
              .set('data',image.date().format('YYYY-MM-dd'))
              .copyProperties(image,image.propertyNames())
})

//Criando uma lista com as datas (serão importantes para definirmos o intervalo das análises)
print("Quais datas das imagen",collection.aggregate_array('data'))
// addlayer mapa
Map.addLayer(collection,  {bands: ['B4', 'B3', 'B2'], min: 0, max: 0.20}, 'Sentinel-2 RGB');


// Definir o ponto de amostragem
//var points = ee.Geometry.Point([-38.90028081713896,-7.642341945881813]); // Coordenadas do ponto 5 metros para frente 
var numPoints = ee.FeatureCollection('projects/ee-geovaneandradesilvatito/assets/clorofilaserrinha')
Map.addLayer(numPoints, {color: 'red'}, 'Ponto de Amostra');
Map.centerObject(points, 10);
print('Primeiros 100', numPoints.toList(50));
///////////// divisão por quartis

/*
var pontosClassificados = numPoints.map(function(f) {
  var cl = ee.Number(f.get('clorofila'));
   /// a classe difinida por divisão quantilica
  var classe = ee.Number.expression (
    "(cl < 14) ? 0 : (cl < 17) ? 1 : (cl < 20) ? 2 : 3", // cl < 14.6 ? 0 → A 
                                                                //clorofila é menor que 14.6? 
                                                               //Se sim, classe 0.
                                                               //: → SENÃO... (abrimos uma nova pergunta)
    {"cl":cl}
    )
  
  return f.set('classe', classe);
});

*/

var listaCl = numPoints.aggregate_array('clorofila');

var percentis = numPoints.reduceColumns({
  reducer: ee.Reducer.percentile([50]),
  selectors: ['clorofila']
});

var p50 = ee.Number(percentis.get('p50'));



var pontosClassificados = numPoints.map(function(f) {
  var cl = ee.Number(f.get('clorofila'));

  var classe = ee.Number.expression(
    "(cl <= p50) ? 1 : 2",
    {cl: cl, p50: p50}
  );

  return f.set('classe', classe);
});

var bandas = [ //procurar as mais importantes
'NDCI',
'B2','B3','B4','B5','B6','B7','B8','B8A','S1','S2','S3','S4','S5','S6','S7','S8',
'CI1','CI2','CI3','CI4','CI5',
'G1','G2',
'R1','R2','R3','R4','R5',
'R6','R7','R8','R9',
'R10','R11','R12',
'R13','R14','R15',
'N1','N2','N3','N4','N5',
'N6','N7','N8',
'N9','N10','N11',
'IN1','IN2','IN3','IN4','IN5'
];

// pixel 3x3
var kernel3x3 = collection.median().select(bandas)
  .reduceNeighborhood(ee.Reducer.median(), ee.Kernel.square(1));
  
  //Volta aos nomes originais (NDCI_median -> NDCI)
  kernel3x3 = kernel3x3.rename(bandas);
  // amostras
  var amostras = kernel3x3.sampleRegions({
  collection: pontosClassificados,
  properties: ['classe'],
  scale: 10
});

// treino e teste
var seed = 38;

// Gera coluna aleatória controlada
var withRandom = amostras.randomColumn('random', seed);

// Embaralha de forma reprodutível
var ordenado = withRandom.sort('random');

// Converte para lista
var lista = ordenado.toList(ordenado.size());

// Split fixo após embaralhamento
var treino = ee.FeatureCollection(lista.slice(0, 27));
var teste  = ee.FeatureCollection(lista.slice(27, 38));

// Conferência
print('Treino:', treino.size()); // 27
print('Teste:', teste.size());   // 11

// Agrupa os pontos por classe e conta quantos existem em cada uma no treino
var contagemPorClasse = treino.aggregate_histogram('classe');
// Exibe o resultado no console
print('Distribuição de pontos por classe treino:', contagemPorClasse);

// teste 
var contagemPorClasse = teste.aggregate_histogram('classe');
// Exibe o resultado no console
print('Distribuição de pontos por classe teste:', contagemPorClasse);

// Random forest
var modelo = ee.Classifier.smileRandomForest({
                        numberOfTrees:500,
                        minLeafPopulation: 2,        // reduz overfitting 2
                        bagFraction: 0.4,
                        maxNodes: 10
                        
                         })
  .train({
    features: treino,
    classProperty: 'classe',
    inputProperties: bandas
  });
  
  var classificado = collection.median()
  .select(bandas)
  .classify(modelo).clip(roi);
  
  Map.addLayer(classificado, {
  min: 0,
  max: 4,
  palette: ['blue', 'yellow', 'red', 'green']
}, 'Classificação Clorofila');


// Matriz de confussão
var validacao = teste.classify(modelo);

var matriz = validacao.errorMatrix('classe', 'classification');
//////////////////metricas de avaliação /////////////////////////////////////////////////////      
print('Matriz de confusão RF', matriz);
print('Acurácia RF', matriz.accuracy());
//var precision = matriz.consumersAccuracy();
//var recall = matriz.producersAccuracy();
//print('Precisão', precision);
//print('Recall', recall);
//print('Kappa RF', matriz.kappa());

var importancia = modelo.explain();
print('Importância das variáveis', importancia);

// ee.Classifier.smileKNN----------------------------------------------------------------------------

var modeloKNN = ee.Classifier.smileKNN({
  k: 7
}).train({
  features: treino,
  classProperty: 'classe',
  inputProperties: bandas
});

var classificadoKNN = collection.median()
  .select(bandas)
  .classify(modeloKNN);
  
  Map.addLayer(classificadoKNN, {
  min: 1,
  max: 3,
  palette: ['blue', 'yellow', 'red']
}, 'Classificação Clorofila knn');


// Matriz de confussão
var validacao = teste.classify(modeloKNN);

var matrizk = validacao.errorMatrix('classe', 'classification');
      
print('Matriz de confusão knn', matrizk);
print('Acurácia knn', matrizk.accuracy());


// ee.Classifier.libsvm------------------------------------------------------------------------------------
var modeloSVM = ee.Classifier.libsvm({
  kernelType: 'RBF',
  gamma: 0.5,
  cost: 10
}).train({
  features: treino,
  classProperty: 'classe',
  inputProperties: bandas
});

var classificadoSVM  = collection.median()
  .select(bandas)
  .classify(modeloSVM);

Map.addLayer(classificadoSVM, {
  min: -0.1,
  max: 2,
  palette: ['blue', 'yellow', 'red']
}, 'SVM');


// Matriz de confussão
var validacao = teste.classify(modeloSVM);

var matrizs = validacao.errorMatrix('classe', 'classification');
      
print('Matriz de confusão SVM', matrizs);
print('Acurácia SVM', matrizs.accuracy());



// ee.Classifier.smileGradientTreeBoost----------------------------------------------------------------------------------
var modeloXGB = ee.Classifier.smileGradientTreeBoost({
  numberOfTrees: 500,
  shrinkage: 0.05,
  maxNodes: 5,
}).train({
  features: treino,
  classProperty: 'classe',
  inputProperties: bandas
});

var classificadox = collection.median()
  .select(bandas)
  .classify(modeloXGB);
  
  Map.addLayer(classificadox, {
  min: 0,
  max: 2,
  palette: ['blue', 'yellow', 'red']
}, 'Classificação Clorofila xx');


// Matriz de confussão
var validacao = teste.classify(modeloXGB);

var matrizx = validacao.errorMatrix('classe', 'classification');
      
print('Matriz de confusão x', matrizx);
print('Acurácia x', matrizx.accuracy());


var classificadoSuavizado = classificado.focal_mode({
  radius: 1, 
  kernelType: 'square', 
  units: 'pixels'
});
Map.addLayer(classificadoSuavizado, {min: 0, max: 3, palette: ['blue', 'yellow', 'red', 'green']}, 'Classificação Suavizada');

//
//
// exportando 
 Export.image.toDrive({
    image: classificado.select("classification"), // agora é uma ee.Image
    description: 'RF3x3',
    folder: 'ee_demos',
    region: roi,
    scale: 10,
    crs: 'EPSG:31984',
    maxPixels: 1e13,
    formatOptions: { cloudOptimized: true }
  });