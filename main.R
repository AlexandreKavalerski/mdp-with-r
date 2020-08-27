T_up = matrix(0,12,12)

T_right = matrix(0,12,12)

T_down = matrix(0,12,12)

T_left = matrix(0,12,12)

T_up[1,1] = 0.1
T_up[1,2] = 0.8
T_up[1,4] = 0.1

T_up[2,2] = 0.1
T_up[2,3] = 0.8
T_up[2,5] = 0.1

T_up[3,3] = 0.9
T_up[3,6] = 0.1

T_up[4,1] = 0.1
T_up[4,5] = 0.8
T_up[4,7] = 0.1

T_up[5,2] = 0.1
T_up[5,6] = 0.8
T_up[5,8] = 0.1

T_up[6,3] = 0.1
T_up[6,6] = 0.8
T_up[6,9] = 0.1

T_up[7,4] = 0.1
T_up[7,8] = 0.8
T_up[7,10] = 0.1

T_up[8,5] = 0.1
T_up[8,9] = 0.8
T_up[8,11] = 0.1

T_up[9,6] = 0.1
T_up[9,9] = 0.8
T_up[9,12] = 0.1

T_up[10,7] = 0.1
T_up[10,10] = 0.1
T_up[10,11] = 0.8

T_right[1,1] = 0.1
T_right[1,2] = 0.1
T_right[1,4] = 0.8

T_right[2,1] = 0.1
T_right[2,3] = 0.1
T_right[2,5] = 0.8

T_right[3,2] = 0.1
T_right[3,3] = 0.1
T_right[3,6] = 0.8

T_right[4,4] = 0.1
T_right[4,5] = 0.1
T_right[4,7] = 0.8

T_right[5,4] = 0.1
T_right[5,6] = 0.1
T_right[5,8] = 0.8

T_right[6,5] = 0.1
T_right[6,6] = 0.1
T_right[6,9] = 0.8

T_right[7,7] = 0.1
T_right[7,8] = 0.1
T_right[7,10] = 0.8

T_right[8,7] = 0.1
T_right[8,9] = 0.1
T_right[8,11] = 0.8

T_right[9,8] = 0.1
T_right[9,9] = 0.1
T_right[9,12] = 0.8

T_right[10,10] = 0.9
T_right[10,11] = 0.1

T_down[1,1] = 0.9
T_down[1,4] = 0.1

T_down[2,1] = 0.8
T_down[2,2] = 0.1
T_down[2,5] = 0.1

T_down[3,2] = 0.8
T_down[3,3] = 0.1
T_down[3,6] = 0.1

T_down[4,1] = 0.1
T_down[4,4] = 0.8
T_down[4,7] = 0.1

T_down[5,2] = 0.1
T_down[5,4] = 0.8
T_down[5,8] = 0.1

T_down[6,3] = 0.1
T_down[6,5] = 0.8
T_down[6,9] = 0.1

T_down[7,4] = 0.1
T_down[7,7] = 0.8
T_down[7,10] = 0.1

T_down[8,5] = 0.1
T_down[8,7] = 0.8
T_down[8,11] = 0.1

T_down[9,6] = 0.1
T_down[9,8] = 0.8
T_down[9,12] = 0.1

T_down[10,7] = 0.1
T_down[10,10] = 0.9


T_left[1,1] = 0.9
T_left[1,2] = 0.1

T_left[2,1] = 0.1
T_left[2,2] = 0.8
T_left[2,3] = 0.1

T_left[3,2] = 0.1
T_left[3,3] = 0.9

T_left[4,1] = 0.8
T_left[4,4] = 0.1
T_left[4,5] = 0.1

T_left[5,2] = 0.8
T_left[5,4] = 0.1
T_left[5,6] = 0.1

T_left[6,3] = 0.8
T_left[6,5] = 0.1
T_left[6,6] = 0.1

T_left[7,4] = 0.8
T_left[7,7] = 0.1
T_left[7,8] = 0.1

T_left[8,5] = 0.8
T_left[8,7] = 0.1
T_left[8,9] = 0.1

T_left[9,6] = 0.8
T_left[9,8] = 0.1
T_left[9,9] = 0.1

T_left[10,7] = 0.8
T_left[10,10] = 0.1
T_left[10,11] = 0.1

update_value <- function(T_up,T_down,T_right,T_left,rw,value){
  value_aux = matrix(0,1,12)  
  
  for (s in 1:12){
    v_up = T_up[s,] %*% t(value)   
    v_down = T_down[s,] %*% t(value)
    v_left = T_left[s,] %*% t(value)
    v_right = T_right[s,] %*% t(value)
    
    # equacao de Bellman aplicada ao estado s
    value_aux[1,s] = rw[1,s] + max(c(v_up,v_down,v_left,v_right))
  }
  value = value_aux
  
  # aqui eh uma funcao para imprimir as utilidades me uma grade
  aux = matrix(0,3,4)
  aux[1,] = c(value[3],value[6],value[9],value[12])
  aux[2,] = c(value[2],value[5],value[8],value[11])
  aux[3,] = c(value[1],value[4],value[7],value[10])
  
  print(aux)
  
  return(value_aux)
  
}

return_policy <- function(T_up,T_down,T_right,T_left,value){
  policy = cbind()
  
  for (s in 1:10){
    
    # calculando a utilidade de cada acao ao estado s  
    v_up = T_up[s,] %*% t(value)   
    v_down = T_down[s,] %*% t(value)
    v_left = T_left[s,] %*% t(value)
    v_right = T_right[s,] %*% t(value)
    
    # escolhendo para o estado s a acao com maxima utilidade esperada
    policy = cbind(policy,which.max(c(v_up,v_down,v_left,v_right)))
    
  }
    # funcao para imprimir a politica em uma grade
    actions = c("UP","DW","LF","RG")
    
    
    
    s1 = paste(actions[policy[3]],actions[policy[6]],actions[policy[9]],"+1")
    s2 = paste(actions[policy[2]],actions[policy[5]],actions[policy[8]],"-1")
    s3 = paste(actions[policy[1]],actions[policy[4]],actions[policy[7]],actions[policy[10]])
    
    cat("\n",s1,"\n",s2,"\n",s3)
    return(policy)
  
}

execution_result <- function(r_value){
  
  rw = matrix(r_value,1,12)
  rw[1,12] = 1
  rw[1,11] = -1
  rw[1,10] = 0.2
  rw[1,5] = -0.5  
  
  # INICIALIZADO OS VALORES DE UTILIDADE COM ZERO
  value = matrix(0,1,12)
  
  # aplicando as equacoes de Bellman durante 10 iteracoes 
  for(i in 1:10){
    value = update_value(T_up,T_down,T_right,T_left,rw,value)
  }
  
  cat("value: ", value)
  
  # retornando a politica final
  policy = return_policy(T_up,T_down,T_right,T_left,value)
}



# EXECUCAO

# INICIALIzACAO DAS RECOMPENSAS
r_value1 = -0.4
r_value2 = -0.04
r_value3 = -0.0004


policy1 = execution_result(r_value1)
policy2 = execution_result(r_value2)
policy3 = execution_result(r_value3)