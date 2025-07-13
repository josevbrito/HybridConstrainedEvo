# Hybrid Evolutionary Algorithms for Constrained Optimization

Este projeto implementa e compara o desempenho de diferentes algoritmos de otimização metaheurística, incluindo Algoritmos Genéticos (AG) com técnicas de clusterização, Particle Swarm Optimization (PSO), Bat Algorithm, um novo algoritmo híbrido BAT-PSO, e um Algoritmo Genético com tratamento de restrições por penalidade dinâmica. O foco principal é a capacidade desses algoritmos de resolver problemas de otimização com restrições.

## 📋 Descrição do Projeto

O objetivo central deste trabalho é desenvolver e avaliar algoritmos híbridos e estratégias de tratamento de restrições para problemas de otimização contínuos. O projeto evolui através de três atividades principais:

### Atividade I - Algoritmos Genéticos com Clusterização
Implementação de variações do Algoritmo Genético utilizando técnicas de clusterização para manter a diversidade da população e evitar convergência prematura em problemas multimodais:
- **AG Padrão**: Implementação clássica do algoritmo genético.
- **AG com K-Means**: Utiliza clusterização K-Means para preservar diversidade.
- **AG com Clustering Hierárquico**: Aplicação de clustering hierárquico.
- **AG com DBSCAN**: Implementação usando densidade-based clustering.

### Atividade II - Comparação com Outros Algoritmos Inspirados em Enxames
Extensão da comparação incluindo meta-heurísticas baseadas em inteligência de enxames:
- **PSO (Particle Swarm Optimization)**: Algoritmo baseado em inteligência de enxames, conhecido por sua convergência rápida em problemas unimodais.
- **Bat Algorithm**: Algoritmo inspirado no comportamento de ecolocalização de morcegos, buscando um equilíbrio entre exploração e explotação.

### Atividade III - Algoritmos Híbridos para Otimização Restrita
Esta atividade foca na construção de um algoritmo híbrido e na integração de mecanismos para lidar com restrições em problemas de otimização:
- **BAT-PSO Híbrido**: Um novo algoritmo que combina as forças de exploração do Bat Algorithm e de explotação do PSO. A hibridização é realizada através de uma estratégia de alternância probabilística de operadores, onde a cada iteração, é decidido probabilisticamente qual lógica de atualização (BA ou PSO) será aplicada ao indivíduo. Os parâmetros do algoritmo foram ajustados para otimizar seu desempenho.
- **GA Penalizado**: Uma adaptação do Algoritmo Genético Padrão que incorpora um mecanismo de **função de penalidade dinâmica** para tratar problemas com restrições. A penalidade, que aumenta ao longo das gerações, é adicionada à função objetivo para soluções que violam restrições, forçando o algoritmo a encontrar soluções viáveis.

## 🎯 Funções de Benchmark

O projeto utiliza três categorias principais de funções de teste para uma avaliação abrangente:

### Funções Monomodais
- **Sphere**: Função esférica simples para teste de convergência básica e explotação.

### Funções Multimodais
- **Schwefel**: Função com múltiplos ótimos locais, utilizada para testar a capacidade de exploração e escape de ótimos locais.

### Problemas com Restrições
- **Michalewicz (Restrito)**: Um problema de otimização com 6 variáveis e restrições lineares de igualdade e não-negatividade, extraído da literatura para otimização restrita. Este problema é crucial para avaliar a eficácia do `GA Penalizado` e demonstrar a importância do tratamento de restrições.

## 🔧 Estrutura do Código

O projeto está organizado nos seguintes arquivos principais:

### Algoritmos Evolutivos e Híbridos
- `GA.R`: Implementações dos AGs (padrão, K-means, hierárquico, DBSCAN) e do `GA Penalizado`.
- `PSO.R`: Implementação do Particle Swarm Optimization.
- `BAT.R`: Implementação do Bat Algorithm.
- `BAT_PSO_Hybrid.R`: Implementação do algoritmo híbrido BAT-PSO.

### Componentes Auxiliares
- `init.population.R`: Funções para inicialização da população.
- `selection.R`: Métodos de seleção (roleta e torneio).
- `crossover.R`: Operadores de crossover (simples e aritmético).
- `mutation.R`: Operador de mutação uniforme.
- `clustering.R`: Implementação das técnicas de clusterização (K-Means, Hierárquico, DBSCAN).

### Funções de Benchmark
- `Benchmarks.R`: Funções de teste monomodais e multimodais (Esfera, Schwefel, Rosenbrock, Ackley, Griewank, Alpine).
- `Benchmarks_Constrained.R`: Funções de teste com restrições (Michalewicz).

### Análise e Execução
- `comparison.R`: Framework de comparação experimental, incluindo análise estatística e tratamento de inviabilidade para problemas restritos.
- `main.R`: Script principal de execução do experimento completo.
- `optimize_BAT_PSO_Hybrid_params.R`: Script auxiliar para otimização de parâmetros do `BAT-PSO Híbrido` (não faz parte da execução principal, mas está incluído no repositório).

## ⚙️ Parâmetros dos Algoritmos

Os parâmetros utilizados para os algoritmos foram os seguintes (para os resultados apresentados no relatório):

### Algoritmos Genéticos (GAs)
- **População**: 50 indivíduos
- **Gerações**: 500
- **Probabilidade de crossover**: 0.8
- **Probabilidade de mutação**: 0.1
- **Frequência de clusterização**: A cada 10 gerações (para AGs com clusterização)
- **Número de clusters**: 5 (para K-Means e Hierárquico)
- **Parâmetros DBSCAN**: eps=0.5, minPts=3

### PSO
- **Partículas**: 50
- **Iterações**: 500
- **Peso de inércia (w)**: 0.9 (com decaimento de 0.99)
- **Coeficientes de aceleração**: c1 = c2 = 2.0

### Bat Algorithm
- **População**: 50 morcegos
- **Iterações**: 500
- **Intensidade sonora inicial (A)**: 0.5
- **Taxa de pulso inicial (r)**: 0.5
- **Faixa de frequência**: [0, 2]
- **Coeficientes**: α = γ = 0.9

### BAT-PSO Híbrido (com parâmetros ajustados)
- **População**: 50 indivíduos
- **Iterações**: 500
- **Parâmetros Bat Algorithm**: A = 0.2422741, r = 0.8776998, Qmin = 0, Qmax = 2, alpha = 0.9295661, gamma = 0.8822333
- **Parâmetros PSO**: w = 0.8339623, c1 = 0.8904942, c2 = 2.2953754, w_damp = 0.99
- **Probabilidade de usar a lógica do Bat Algorithm ($P_{bat}$)**: 0.3877847

### GA Penalizado
- **População**: 50 indivíduos
- **Gerações**: 500
- **Probabilidade de crossover**: 0.8
- **Probabilidade de mutação**: 0.1
- **Parâmetros de penalidade dinâmica**: k_penalty = 0.1, p_penalty = 1.0

## 📊 Metodologia Experimental

### Configuração dos Testes
- **Execuções independentes**: 30 por algoritmo/função, para garantir robustez estatística.
- **Dimensão do problema**: 10 para funções não-restritas (Sphere, Schwefel) e 6 para o problema restrito (Michalewicz).
- **Seed aleatória**: 123 (para reprodutibilidade dos experimentos).

### Funções Testadas
1.  **Sphere** (Monomodal): Domínio $[-5.12, 5.12]$
2.  **Schwefel** (Multimodal): Domínio $[-500, 500]$
3.  **Michalewicz** (Restrito): Variáveis $x_i \in [0, 5]$, com restrições de igualdade e não-negatividade.

### Análise Estatística
- **Estatísticas Descritivas**: Média, desvio padrão, mediana, min/max dos resultados finais.
- **Ranking Médio**: Classificação de desempenho por função e geral.
- **ANOVA (Analysis of Variance)**: Teste de significância entre grupos para identificar diferenças nas médias.
- **Tukey HSD (Honestly Significant Difference)**: Teste post-hoc para comparações pareadas, aplicado se a ANOVA indicar significância.
- **Tratamento de Inviabilidade**: Para o problema restrito, algoritmos sem tratamento intrínseco de restrições têm soluções inviáveis penalizadas com um fitness de $10^9$ para fins de comparação, refletindo sua incapacidade de resolver o problema restrito.

## 📈 Visualizações

O sistema gera automaticamente gráficos em formato PNG para cada função:
- **Gráficos de convergência**: Evolução do melhor fitness ao longo das gerações.
- **Box plots**: Distribuição dos resultados finais das 30 execuções.

## 🚀 Como Executar

### Pré-requisitos
Certifique-se de ter o R instalado. Além disso, instale o pacote `dbscan` se ainda não o tiver:
```r
install.packages("dbscan")
```

### Execução
Para rodar o experimento completo e gerar os resultados e gráficos, execute o script `main.R` no seu ambiente R:

```r
source("main.R")
```

### Saída Esperada
O programa imprimirá a análise estatística completa no console. Além disso, os seguintes arquivos PNG serão gerados na pasta raiz do projeto:

- `convergencia_Esfera_(Monomodal).png`
- `boxplot_Esfera_(Monomodal).png`
- `convergencia_Schwefel_(Multimodal).png`
- `boxplot_Schwefel_(Multimodal).png`
- `convergencia_Michalewicz_(Restrito).png`
- `boxplot_Michalewicz_(Restrito).png`

## 🎯 Insights Obtidos

Este projeto demonstra a importância da escolha do algoritmo e da abordagem para problemas de otimização, especialmente aqueles com restrições.

### Algoritmos Híbridos (BAT-PSO)
- O `BAT-PSO Híbrido` demonstrou excelente desempenho em funções monomodais, igualando o PSO.
- Para funções multimodais, o `BAT-PSO Híbrido` mostrou uma melhora significativa em relação às suas versões anteriores e ao Bat Algorithm original, evidenciando a eficácia da combinação de operadores e do ajuste de parâmetros.

### Tratamento de Restrições (GA Penalizado)
- O `GA Penalizado` foi o único algoritmo capaz de encontrar soluções válidas para o problema de otimização restrita, destacando a necessidade e a eficácia de mecanismos específicos para lidar com restrições.
- Algoritmos sem tratamento de restrições falham em encontrar soluções viáveis para problemas restritos, resultando em fitness muito altos (penalizados) para fins de comparação.

### Comparação Geral
- Não existe um "melhor" algoritmo para todos os problemas (Teorema No Free Lunch). O PSO é excelente para explotação em problemas unimodais. AGs com clusterização são robustos para exploração em problemas multimodais. Para problemas restritos, um mecanismo dedicado é indispensável.

## 🛠️ Possíveis Extensões

1.  **Ajuste Fino de Parâmetros**: Realizar uma otimização mais sofisticada dos hiperparâmetros (além do Random Search básico) para o `BAT-PSO Híbrido` e `GA Penalizado`.
2.  **Estratégias de Hibridização Avançadas**: Explorar outras formas de combinar meta-heurísticas (ex: co-evolução, adaptação dinâmica de pesos de operadores).
3.  **Outras Abordagens de Restrições**: Implementar e comparar métodos de tratamento de restrições como operadores fechados (GENOCOP), reparo de soluções ou métodos de projeção.
4.  **Mais Funções de Benchmark**: Testar os algoritmos em um conjunto mais amplo de funções de benchmark (incluindo mais problemas restritos do conjunto CEC).
5.  **Aplicação em Problemas Reais**: Utilizar os algoritmos desenvolvidos para resolver problemas de otimização do mundo real com restrições.
