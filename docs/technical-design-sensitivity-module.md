# UKBAnalytica 敏感性分析模块技术设计文档

## 1. 概述

**版本**: 0.6.0 设计草案  
**作者**: UKBAnalytica 开发团队  
**日期**: 2026-04-06

---

## 2. 背景

当前包已经支持主分析回归接口：

- `runmulti_lm()`
- `runmulti_logit()`
- `runmulti_cox()`

这些函数的接口已经比较稳定，且都以 `data.frame` / `data.table` 作为输入。在不改动主分析模块的前提下，需要新增一个轻量的敏感性分析模块，用于在回归前先对子集样本进行标准化筛选，再把筛选后的数据直接传回现有回归函数。

本次设计覆盖两个常用敏感性分析场景：

1. 去除随访前 `n` 年内发病的个体
2. 去除指定协变量中任一为 `NA` 的个体

---

## 3. 设计目标

### 3.1 目标

1. 新模块只负责数据筛选，不负责拟合模型
2. 不修改 `regression.R` 中现有接口
3. 返回对象可无缝传入 `runmulti_lm()`、`runmulti_logit()`、`runmulti_cox()`
4. 输入和输出尽量保持同构：同列名、同类、同编码方式
5. 行为可追踪：保留筛选摘要信息，便于方法学报告

### 3.2 非目标

1. 不在本模块内重新实现回归函数
2. 不引入新的公式接口
3. 不自动处理全部缺失值
4. 不替代多重插补模块

---

## 4. 与现有模块的兼容依据

现有 `runmulti_*()` 回归函数的输入验证非常简单，核心要求只有两点：

1. `data` 必须是 `data.frame` 或 `data.table`
2. 模型所需列必须存在

因此，只要敏感性分析函数满足以下契约，就不需要修改主模块：

1. 返回对象仍然是 `data.frame` / `data.table`
2. 不删除非必要列
3. 不重命名现有列
4. 只做行筛选或保守的属性附加

结论：敏感性分析模块应当被设计为“数据预处理层”，而不是“回归包装层”。

---

## 5. 模块文件规划

建议新增文件：

```text
R/sensitivity.R
```

建议提供两个导出函数和两个内部辅助函数：

```text
R/sensitivity.R
├── sensitivity_exclude_early_events()       # 导出
├── sensitivity_exclude_missing_covariates() # 导出
├── .validate_sensitivity_inputs()           # 内部
└── .attach_sensitivity_metadata()           # 内部
```

---

## 6. API 设计

### 6.1 `sensitivity_exclude_early_events()`

#### 6.1.1 功能

去除在随访前 `n` 年内发生结局事件的样本，用于降低反向因果或潜在潜伏期造成的偏倚。

#### 6.1.2 函数签名

```r
sensitivity_exclude_early_events(
  data,
  endpoint = c("outcome_surv_time", "outcome_status"),
  n_years,
  copy = TRUE,
  verbose = TRUE
)
```

#### 6.1.3 参数说明

| 参数 | 类型 | 说明 |
|------|------|------|
| `data` | data.frame / data.table | 输入分析数据 |
| `endpoint` | character(2) | 生存时间与结局状态列，格式为 `c(time_col, status_col)` |
| `n_years` | numeric(1) | 要排除的前 `n` 年窗口，必须大于 0 |
| `copy` | logical(1) | 是否先复制对象再筛选，默认 `TRUE` |
| `verbose` | logical(1) | 是否打印筛选摘要 |

#### 6.1.4 适用前提

1. `endpoint[1]` 表示随访时间，单位默认按“年”解释
2. `endpoint[2]` 使用现有包约定：`1 = event`，`0 = censored`
3. `NA` 状态值默认保留，不在本函数内强制删除

#### 6.1.5 核心规则

删除满足以下条件的样本：

- `status == 1` 且 `time <= n_years`

保留以下样本：

- `status == 0`
- `status` 缺失
- `time` 缺失
- `status == 1` 且 `time > n_years`

这里选择“只删除明确定义的早发事件”，不额外处理缺失，是为了最大限度保持该函数为一个可预测、单一职责的数据筛选器。

#### 6.1.6 返回值

返回与输入同类、同列结构的数据对象：

- 输入为 `data.table`，输出仍为 `data.table`
- 输入为 `data.frame`，输出仍为 `data.frame`
- 所有原始列完整保留
- 仅删除符合条件的行

同时建议附加属性：

```r
attr(result, "sensitivity_info")
```

建议结构：

```r
list(
  method = "exclude_early_events",
  endpoint = c("outcome_surv_time", "outcome_status"),
  n_years = 2,
  n_input = 100000,
  n_removed = 356,
  n_output = 99644
)
```

#### 6.1.7 伪代码

```r
time_col <- endpoint[1]
status_col <- endpoint[2]

drop_idx <- !is.na(data[[status_col]]) &
            data[[status_col]] == 1 &
            !is.na(data[[time_col]]) &
            data[[time_col]] <= n_years

if (data.table::is.data.table(data)) {
  result <- data[!drop_idx]
} else {
  result <- data[!drop_idx, , drop = FALSE]
}

result <- .attach_sensitivity_metadata(...)
```

#### 6.1.8 示例

```r
dt_sens <- sensitivity_exclude_early_events(
  data = analysis_dt,
  endpoint = c("outcome_surv_time", "outcome_status"),
  n_years = 2
)

runmulti_cox(
  data = dt_sens,
  main_var = c("BMI", "SBP"),
  covariates = c("age", "sex"),
  endpoint = c("outcome_surv_time", "outcome_status")
)
```

---

### 6.2 `sensitivity_exclude_missing_covariates()`

#### 6.2.1 功能

删除指定协变量集合中“任一协变量为 `NA`”的个体。

该函数对指定协变量执行 complete case 筛选，但只针对 `covariates` 参数中传入的列，不扩展到主暴露、结局或其他非指定变量。

#### 6.2.2 函数签名

```r
sensitivity_exclude_missing_covariates(
  data,
  covariates,
  copy = TRUE,
  verbose = TRUE
)
```

#### 6.2.3 参数说明

| 参数 | 类型 | 说明 |
|------|------|------|
| `data` | data.frame / data.table | 输入分析数据 |
| `covariates` | character vector | 需要检查的协变量列名 |
| `copy` | logical(1) | 是否先复制对象再筛选，默认 `TRUE` |
| `verbose` | logical(1) | 是否打印筛选摘要 |

#### 6.2.4 核心规则

删除满足以下条件的样本：

- 指定 `covariates` 中任意一列为 `NA`

保留以下样本：

- 指定 `covariates` 中所有列都非缺失

#### 6.2.5 返回值

返回与输入同类、同列结构的数据对象，且建议附加如下元信息：

```r
list(
  method = "exclude_missing_covariates",
  covariates = c("age", "sex", "bmi", "smoking"),
  n_input = 100000,
  n_removed = 124,
  n_output = 99876
)
```

#### 6.2.6 伪代码

```r
if (data.table::is.data.table(data)) {
  cov_dt <- data[, ..covariates]
} else {
  cov_dt <- data[, covariates, drop = FALSE]
}

drop_idx <- !stats::complete.cases(cov_dt)

if (data.table::is.data.table(data)) {
  result <- data[!drop_idx]
} else {
  result <- data[!drop_idx, , drop = FALSE]
}

result <- .attach_sensitivity_metadata(...)
```

#### 6.2.7 示例

```r
dt_sens <- sensitivity_exclude_missing_covariates(
  data = analysis_dt,
  covariates = c("age", "sex", "bmi", "smoking")
)

runmulti_logit(
  data = dt_sens,
  main_var = c("BMI", "SBP"),
  covariates = c("age", "sex", "bmi", "smoking"),
  outcome = "disease_status"
)
```

---

## 7. 内部辅助函数设计

### 7.1 `.validate_sensitivity_inputs()`

统一完成以下校验：

1. `data` 是否为 `data.frame` / `data.table`
2. 目标列是否存在
3. `n_years` 是否为单个正数
4. `covariates` 是否为非空字符向量

建议签名：

```r
.validate_sensitivity_inputs <- function(
  data,
  required_cols,
  n_years = NULL,
  covariates = NULL
)
```

### 7.2 `.attach_sensitivity_metadata()`

为结果对象附加可审计的筛选信息，但不修改原始列结构。

建议签名：

```r
.attach_sensitivity_metadata <- function(data, info)
```

设计要求：

1. 仅增加属性，不增加列
2. 不改变对象可被回归函数识别的行为
3. 如已有 `sensitivity_info` 属性，合并为列表链式记录

---

## 8. 数据契约

为确保与现有回归模块无缝兼容，输出对象必须遵守以下契约：

| 约束 | 要求 |
|------|------|
| 对象类型 | 保持 `data.frame` 或 `data.table` |
| 列名 | 全部保留，不重命名 |
| 列类型 | 不主动转换 |
| 行顺序 | 默认保持原顺序 |
| 缺失值 | 除命中筛选规则的行外，不主动删除 |
| 属性 | 允许附加，不影响模型拟合 |

---

## 9. 与现有回归模块的集成方式

### 9.1 设计原则

不修改以下函数：

- `runmulti_lm()`
- `runmulti_logit()`
- `runmulti_cox()`

敏感性分析函数仅作为前置步骤使用：

```r
dt_sens <- sensitivity_exclude_early_events(...)
res <- runmulti_cox(data = dt_sens, ...)
```

或：

```r
dt_sens <- sensitivity_exclude_missing_covariates(...)
res <- runmulti_lm(data = dt_sens, ...)
```

### 9.2 组合使用

两个函数应支持串联：

```r
dt_sens <- analysis_dt |>
  sensitivity_exclude_early_events(
    endpoint = c("outcome_surv_time", "outcome_status"),
    n_years = 2
  ) |>
  sensitivity_exclude_missing_covariates(
    covariates = c("age", "sex", "bmi", "smoking")
  )
```

随后可以直接进入现有回归函数：

```r
runmulti_cox(
  data = dt_sens,
  main_var = c("BMI", "LDL"),
  covariates = c("age", "sex", "bmi", "smoking"),
  endpoint = c("outcome_surv_time", "outcome_status")
)
```

---

## 10. 边界条件与行为约定

### 10.1 `sensitivity_exclude_early_events()`

1. `n_years <= 0` 时直接报错
2. `endpoint` 长度不为 2 时直接报错
3. 时间列存在负值时直接报错
4. `status = NA` 的样本默认保留
5. `status = 1` 但时间缺失的样本默认保留

说明：

第 4 和第 5 条是保守策略。该函数只处理“确定的早发事件”，不处理无法判定的记录。

### 10.2 `sensitivity_exclude_missing_covariates()`

1. `covariates` 为空时报错
2. 任一协变量列不存在时报错
3. 只要任一指定协变量为 `NA`，即删除该样本
4. 仅当所有指定协变量都非缺失时才保留

说明：

这属于“针对指定协变量集合的 complete case 筛选”。它不会自动检查 `main_var`、`outcome` 或 `endpoint` 中的缺失，因此仍然比“对整套建模变量做 complete case”更窄。

---

## 11. 与现有缺失值策略的关系

当前文档和示例中，回归分析通常采用 complete case 思路，但这是在分析脚本层面完成的，不是通过统一的包内预处理函数完成的。

本模块与 complete case 的关系如下：

| 方案 | 删除规则 | 用途 |
|------|----------|------|
| `sensitivity_exclude_missing_covariates()` | 指定协变量任一缺失即删除 | 协变量完整性敏感性分析 |
| complete case | 任一建模变量缺失即删除 | 严格建模前清洗 |
| multiple imputation | 不直接删除 | 缺失值推断 |

因此，这个新函数本质上是“局部 complete case”，不会替代更严格的全模型 complete case，也不会替代 MI。

---

## 12. 测试设计

建议新增测试文件：

```text
tests/testthat/test-sensitivity.R
```

### 12.1 `sensitivity_exclude_early_events()` 测试点

1. 能正确删除 `status == 1 & time <= n_years` 的行
2. 不删除 `status == 0` 的行
3. 不删除 `status == 1 & time > n_years` 的行
4. 保留 `status == NA` 的行
5. 保持原列结构不变
6. 返回结果可直接传给 `runmulti_cox()`

### 12.2 `sensitivity_exclude_missing_covariates()` 测试点

1. 能正确删除任一指定协变量缺失的行
2. 仅当指定协变量全部非缺失时保留
3. 保持原列结构不变
4. 返回结果可直接传给 `runmulti_lm()` / `runmulti_logit()`

### 12.3 集成测试

1. 两个函数串联后仍可传入 `runmulti_cox()`
2. `data.table` 输入输出类型保持不变
3. `data.frame` 输入输出类型保持不变

---

## 13. 开发步骤建议

### 阶段一：最小可用实现

1. 新增 `R/sensitivity.R`
2. 实现两个导出函数
3. 实现输入校验和属性附加
4. 新增 `test-sensitivity.R`

### 阶段二：文档与示例

1. 为两个函数补充 roxygen 文档
2. 在 `docs/07-main-analysis.Rmd` 或 `docs/08-advanced-analysis.Rmd` 加入敏感性分析示例
3. 在 `README.md` 增加一段简短用法

### 阶段三：可选增强

1. 增加 `summary()` 或 `print()` 方法打印筛选摘要
2. 增加链式摘要函数，例如 `get_sensitivity_info()`
3. 未来可扩展更多敏感性分析预处理器

---

## 14. 推荐实现结论

本模块推荐采用“最小侵入式预处理设计”：

1. 新增 `R/sensitivity.R`
2. 导出两个函数：
   - `sensitivity_exclude_early_events()`
   - `sensitivity_exclude_missing_covariates()`
3. 不修改 `regression.R`
4. 返回与输入同构的数据对象
5. 通过属性记录筛选信息，而不是改写主分析接口

这种方案的优点是：

1. 对现有代码影响最小
2. 使用方式直观
3. 易于写单元测试
4. 适合后续继续扩展更多敏感性分析筛选器

---

## 15. 典型使用范式

```r
# 1. 基于前 2 年事件排除
dt_sens1 <- sensitivity_exclude_early_events(
  data = analysis_dt,
  endpoint = c("outcome_surv_time", "outcome_status"),
  n_years = 2
)

# 2. 再去除任一指定协变量缺失的人
dt_sens2 <- sensitivity_exclude_missing_covariates(
  data = dt_sens1,
  covariates = c("age", "sex", "bmi", "smoking")
)

# 3. 直接进入现有回归模块
res <- runmulti_cox(
  data = dt_sens2,
  main_var = c("BMI", "SBP"),
  covariates = c("age", "sex", "bmi", "smoking"),
  endpoint = c("outcome_surv_time", "outcome_status")
)
```

该范式满足本次需求的核心约束：

1. 接口清晰
2. 与主模块解耦
3. 返回结果可直接进入普通回归分析
