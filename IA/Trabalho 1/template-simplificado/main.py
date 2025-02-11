
import random

from collections import deque
from viewer import MazeViewer
from math import inf, sqrt



def gera_labirinto(n_linhas, n_colunas, inicio, goal):
    # cria labirinto vazio
    labirinto = [[0] * n_colunas for _ in range(n_linhas)]

    # adiciona celulas ocupadas em locais aleatorios de
    # forma que 50% do labirinto esteja ocupado
    numero_de_obstaculos = int(0.40 * n_linhas * n_colunas)
    for _ in range(numero_de_obstaculos):
        linha = random.randint(0, n_linhas-1)
        coluna = random.randint(0, n_colunas-1)
        labirinto[linha][coluna] = 1

    # remove eventuais obstaculos adicionados na posicao
    # inicial e no goal
    labirinto[inicio.y][inicio.x] = 0
    labirinto[goal.y][goal.x] = 0

    return labirinto


class Celula:
    def __init__(self, y, x, anterior):
        self.y = y
        self.x = x
        self.anterior = anterior

# h_score
def distancia(celula_1, celula_2):
    dx = celula_1.x - celula_2.x
    dy = celula_1.y - celula_2.y
    return sqrt(dx ** 2 + dy ** 2)


def esta_contido(lista, celula):
    for elemento in lista:
        if (elemento.y == celula.y) and (elemento.x == celula.x):
            return True
    return False


def custo_caminho(caminho):
    if len(caminho) == 0:
        return inf

    custo_total = 0
    for i in range(1, len(caminho)):
        custo_total += distancia(caminho[i].anterior, caminho[i])

    return custo_total


def obtem_caminho(goal):
    caminho = []

    celula_atual = goal
    while celula_atual is not None:
        caminho.append(celula_atual)
        celula_atual = celula_atual.anterior

    # o caminho foi gerado do final para o
    # comeco, entao precisamos inverter.
    caminho.reverse()

    return caminho


def celulas_vizinhas_livres(celula_atual, labirinto):
    # generate neighbors of the current state
    vizinhos = [
        Celula(y=celula_atual.y-1, x=celula_atual.x-1, anterior=celula_atual),
        Celula(y=celula_atual.y+0, x=celula_atual.x-1, anterior=celula_atual),
        Celula(y=celula_atual.y+1, x=celula_atual.x-1, anterior=celula_atual),
        Celula(y=celula_atual.y-1, x=celula_atual.x+0, anterior=celula_atual),
        Celula(y=celula_atual.y+1, x=celula_atual.x+0, anterior=celula_atual),
        Celula(y=celula_atual.y+1, x=celula_atual.x+1, anterior=celula_atual),
        Celula(y=celula_atual.y+0, x=celula_atual.x+1, anterior=celula_atual),
        Celula(y=celula_atual.y-1, x=celula_atual.x+1, anterior=celula_atual),
    ]

    # seleciona as celulas livres
    vizinhos_livres = []
    for v in vizinhos:
        # verifica se a celula esta dentro dos limites do labirinto
        if (v.y < 0) or (v.x < 0) or (v.y >= len(labirinto)) or (v.x >= len(labirinto[0])):
            continue
        # verifica se a celula esta livre de obstaculos.
        if labirinto[v.y][v.x] == 0:
            vizinhos_livres.append(v)

    return vizinhos_livres


def breadth_first_search(labirinto, inicio, goal, viewer):
    # nos gerados e que podem ser expandidos (vermelhos)
    fronteira = deque()
    # nos ja expandidos (amarelos)
    expandidos = set()

    # adiciona o no inicial na fronteira
    fronteira.append(inicio)

    # variavel para armazenar o goal quando ele for encontrado.
    goal_encontrado = None

    # Repete enquanto nos nao encontramos o goal e ainda
    # existem para serem expandidos na fronteira. Se
    # acabarem os nos da fronteira antes do goal ser encontrado,
    # entao ele nao eh alcancavel.
    while (len(fronteira) > 0) and (goal_encontrado is None):

        # seleciona o no mais antigo para ser expandido
        no_atual = fronteira.popleft()

        # busca os vizinhos do no
        vizinhos = celulas_vizinhas_livres(no_atual, labirinto)

        # para cada vizinho verifica se eh o goal e adiciona na
        # fronteira se ainda nao foi expandido e nao esta na fronteira
        for v in vizinhos:
            if v.y == goal.y and v.x == goal.x:
                goal_encontrado = v
                # encerra o loop interno
                break
            else:
                if (not esta_contido(expandidos, v)) and (not esta_contido(fronteira, v)):
                    fronteira.append(v)

        expandidos.add(no_atual)

        viewer.update(generated=fronteira, expanded=expandidos)
        #viewer.pause()


    caminho = obtem_caminho(goal_encontrado)
    custo   = custo_caminho(caminho)

    return caminho, custo, expandidos


def depth_first_search(labirinto, inicio, goal, viewer):
    # nos gerados e que podem ser expandidos (vermelhos)
    fronteira = deque()
    # nos ja expandidos (amarelos)
    expandidos = set()

    # adiciona o no inicial na fronteira
    fronteira.append(inicio)

    # variavel para armazenar o goal quando ele for encontrado.
    goal_encontrado = None


    while (len(fronteira) > 0) and (goal_encontrado is None):

        # seleciona o no para ser expandido
        no_atual = fronteira.pop()

        # busca os vizinhos do no
        vizinhos = celulas_vizinhas_livres(no_atual, labirinto)

        # para cada vizinho verifica se eh o goal e adiciona na
        # fronteira se ainda nao foi expandido e nao esta na fronteira
        for v in vizinhos:
            if v.y == goal.y and v.x == goal.x:
                goal_encontrado = v
                # encerra o loop interno
                break
            else:
                if (not esta_contido(expandidos, v)) and (not esta_contido(fronteira, v)):
                    fronteira.append(v)

        expandidos.add(no_atual)

        viewer.update(generated=fronteira, expanded=expandidos)
        #viewer.pause()


    caminho = obtem_caminho(goal_encontrado)
    custo   = custo_caminho(caminho)

    return caminho, custo, expandidos


def a_star_search(labirinto, inicio, goal, viewer):
    # Fila prioritaria com (celula, custof)  
    fronteira = deque([(0, inicio)])
    
    # nos ja expandidos (amarelos)
    expandidos = set()

    # Dicionário para armazenar o custo para chegar a cada nó
    custo_ate_agora = {inicio: 0}

    # dicionario para armazenar o caminho
    caminho = {}

    # Acompanhar os nós expandidos
    expandidos = set()

    # Armazenamento do goal quando encontrado
    goal_encontrado = None

    while fronteira:
        # Classifica a fronteira pelo custo
        fronteira = deque(sorted(fronteira, key=lambda x: x[0]))
        # Obtem o nó com o menor custo da fronteira
        _, no_atual = fronteira.popleft()

        # Se econtrar o destino pausa o loop
        if no_atual.y == goal.y and no_atual.x == goal.x:
            goal_encontrado = no_atual
            break

        # Adiciona o nó atual a expandidos
        expandidos.add(no_atual)

        # Obtem os vizinhos livres
        vizinhos = celulas_vizinhas_livres(no_atual, labirinto)

        for vizinho in vizinhos:
            # Calcula o novo custo de cada nó
            novo_custo = custo_ate_agora[no_atual] + distancia(no_atual, vizinho)

            # Se o vizinho não foi visitado ou um caminho mais curto foi achado
            if vizinho not in custo_ate_agora or novo_custo < custo_ate_agora[vizinho]:
                # Update the cost to reach the neighbor
                custo_ate_agora[vizinho] = novo_custo
                # Calculate the priority based on the new cost and heuristic
                prioridade = novo_custo + distancia(vizinho, goal)
                # Add the neighbor to the frontier with the calculated priority
                fronteira.append((prioridade, vizinho))
                # Atualiza o caminho
                caminho[vizinho] = no_atual
                
        #viewer.update(generated=fronteira, expanded=expandidos)
        #viewer.pause()

    caminho = obtem_caminho(goal_encontrado)
    custo   = custo_caminho(caminho)

    return caminho, custo, expandidos
#-------------------------------


def main():
    for _ in range(10):
        #SEED = 21  # coloque None no lugar do 42 para deixar aleatorio
        #random.seed(SEED)
        N_LINHAS  = 200
        N_COLUNAS = 200
        INICIO = Celula(y=0, x=0, anterior=None)
        GOAL   = Celula(y=N_LINHAS-1, x=N_COLUNAS-1, anterior=None)


        """
        O labirinto sera representado por uma matriz (lista de listas)
        em que uma posicao tem 0 se ela eh livre e 1 se ela esta ocupada.
        """
        labirinto = gera_labirinto(N_LINHAS, N_COLUNAS, INICIO, GOAL)
        
    
        viewer = MazeViewer(labirinto, INICIO, GOAL,
                            step_time_miliseconds=20, zoom=40)
        
        #----------------------------------------
        # A-Star Search
        #----------------------------------------
        viewer._figname = "A-Star"
        caminho, custo_total, expandidos = \
            a_star_search(labirinto, INICIO, GOAL, viewer)

        if len(caminho) == 0:
            print("Goal é inalcançavel neste labirinto.")

        print(
            f"A-Star:"
            f"\tCusto total do caminho: {custo_total}.\n"
            f"\tNumero de passos: {len(caminho)-1}.\n"
            f"\tNumero total de nos expandidos: {len(expandidos)}.\n\n"

        ) 

        viewer.update(path=caminho)
        #viewer.pause()
    
    
        #----------------------------------------
        # BFS Search
        #----------------------------------------
        viewer._figname = "BFS"
        caminho, custo_total, expandidos = \
            breadth_first_search(labirinto, INICIO, GOAL, viewer)

        if len(caminho) == 0:
            print("Goal é inalcançavel neste labirinto.")

        print(
            f"BFS:"
            f"\tCusto total do caminho: {custo_total}.\n"
            f"\tNumero de passos: {len(caminho)-1}.\n"
            f"\tNumero total de nos expandidos: {len(expandidos)}.\n\n"

        )

        viewer.update(path=caminho)
        #viewer.pause()
        
    
        #----------------------------------------
        # DFS Search
        #----------------------------------------
        viewer._figname = "DFS"
        caminho, custo_total, expandidos = \
            depth_first_search(labirinto, INICIO, GOAL, viewer)

        if len(caminho) == 0:
            print("Goal é inalcançavel neste labirinto.")

        print(
            f"DFS:"
            f"\tCusto total do caminho: {custo_total}.\n"
            f"\tNumero de passos: {len(caminho)-1}.\n"
            f"\tNumero total de nos expandidos: {len(expandidos)}.\n\n"

        )

        viewer.update(path=caminho)
        #viewer.pause()

        #----------------------------------------
        # Uniform Cost Search (Obs: opcional)
        #---------------------------------------- 
        

        print("OK! Pressione alguma tecla pra finalizar...")
        input()


if __name__ == "__main__":
    main()
