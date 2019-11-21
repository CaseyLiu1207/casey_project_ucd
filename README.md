# casey_project_ucd

taskStartAssigned = taskStartAssigned %>% separate_rows(task, sep = ",")

rownames(greenhouse_task_duration) = greenhouse_task_duration$Crop
colnames(greenhouse_task_duration) = c("Crop", "Sowing", "Thinning", 
                                       "Sampling", "GH Set Up", "Pretreat",
                                       "Transplant Date", "Post Transplant", "Pruning/Tying",
                                       "Flowering/Pollenate", "Harvest")

taskStartAssigned$duration = NA
row_num = dim(taskStartAssigned)[1]

for (i in 1: row_num){
    crop_one = toString(taskStartAssigned$Crop0[i])
    task_one  = taskStartAssigned$task[i]
    if (task_one %in% colnames(greenhouse_task_duration)){
      taskStartAssigned$duration[i] = greenhouse_task_duration[crop_one, task_one]
    }else{
      taskStartAssigned$duration[i] = 1
    }
}

taskStartAssigned_expand = taskStartAssigned %>% mutate(duration = case_when(
                                                        taskStartAssigned$duration == 1 ~ list(c(1:1)),
                                                        taskStartAssigned$duration == 5 ~ list(c(1:5)),
                                                        taskStartAssigned$duration == 30 ~ list(c(1:30)),
                                                        taskStartAssigned$duration == 25 ~ list(c(1:25)),
                                                        taskStartAssigned$duration == 14 ~ list(c(1:14)),
                                                        taskStartAssigned$duration == 21 ~ list(c(1:21)),
                                                        taskStartAssigned$duration == 28 ~ list(c(1:28)),
                                                        taskStartAssigned$duration == 32 ~ list(c(1:32)),
                                                        taskStartAssigned$duration == 40 ~ list(c(1:40)),
                                                        taskStartAssigned$duration == 20 ~ list(c(1:20)),
                                                        taskStartAssigned$duration == 10 ~ list(c(1:10)),
                                                        taskStartAssigned$duration == 42 ~ list(c(1:42)),
                                                        taskStartAssigned$duration == 90 ~ list(c(1:90)))
                                                         )
taskStartAssigned_expand = unnest(taskStartAssigned_expand)
