# Haskell calculator
一个使用 Haskell 编写的计算器, 支持四则运算、一元运算符和括号，处理除零异常。
# 示例
```haskell
main :: IO ()
main = print (calculate "1 + 2 * (-2 + 3) + 2 / (3 - 1)")
```
输出:
```shell
Just 4.0
```
对于除以 0:
```haskell
main :: IO ()
main = print (calculate "1 + 2 * (-2 + 3) + 2 / (3 - 3)")
```
输出:
```shell
Nothing
```