#rna_de_um_neuronio
#testando uma RNA de 1 neuronio

import math

#declaração da entrada e do valor esperado de saida
input = 1
output_desire = 0

#declaração do peso, que é a importancia que se da as entradas
input_weight = 0.5

#taxa de aprendizado - ##HINT: Testar também aumentando a taxa de aprendizado para 0.1 e observar as iteracoes
learning_rate = 0.1

#função de ativação que testa o que é esperado na saida dado o valor desejado
def activation(sum):
    if sum >= 0:
        return 1
    else: 
        return 0 

print("entrada", input, "desejado", output_desire)

#o erro é infinito, pois qualquer erro é menor que o infinito
error = math.inf

#qual é a demora? Quanto tempo ele demora pra aprender? 
iteration = 0

#caso a saida esperada seja 0, é necessario adicionar o neuronio virtual de valor fixo
bias = 1
bias_weight = 0.5

#como é necessario atualizar a taxa de aprendizado até obter o valor desejado, precisa entrar no loop até  validar o desejado 
while not error == 0:
    iteration += 1
    print("######## Iteracao: ", iteration)
    print("peso:", input_weight)
    
    #fazer uma soma de neuronios, o bias resolve o problema da validação do 0
    sum = (input * input_weight) + (bias * bias_weight)

    #nossa saida desejada após a ativação do neuronio
    output = activation (sum)

    print("Saida:", output)

    #A saida está diferente do desejado, logo existe um erro. O erro é igual ao valor desejado - valor real
    error = output_desire - output

    print("erro:", error)

    '''Queremos que o erro seja 0, logo é necessario atualizar os 
    pesos (importancia dada as entradas) juntamente com uma taxa de aprendizado,
    um numero definido no inicio para atualizar a velocidade 
    de aprendizado. Se a TA for muito grande a TA é rapida e se for pequeno a TA é baixa
    se for muito rapida, talvez nao aprenda nada e se for muito devagar, talvez não aprenda nunca. 
    '''
    if not error == 0:

        input_weight = input_weight + learning_rate * input * error
        bias_weight = bias_weight + (learning_rate * bias * error)

print("PARABENS A REDE APRENDEU!!")