---
layout: post
title: golang context 使用注意事项
categories: golang
---

golang 开发中在处理一个请求或一个任务时, 通常会开一个或多个 goroutine
来运行. 如果任务已超时或者请求被取消, 就需要通知运行的 goroutine 及时
结束. context 就是用来通知同一情境下的运行 goroutine 的标准工具. 用法
和示例可以参考 [Package context](https://golang.org/pkg/context/)


这里说一下 context 使用中一些需要注意的地方.

1. 创建 context 的函数结束并调用了 cancel 的话, 所有接收了这个context
   的 goroutine 都会收到 cancel 通知.

比如

```go
func foo() {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	go dosometing(ctx, otherArgs)
}
```

如果 `dosomething` 处理了 context, 那么就会立刻结束. 显然有时候这不是
想要的结果, 所以需要注意 context 的生命周期. 如果任务本身就是设计成异
步处理的话, 可以由 `dosomething` 独立管理自己的 context.


2. 处理 context timeout 的时候, 需要注意时间粒度.

如

```go
func foo(ctx context.Context) error {
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		default:
			dostep()
		}
	}
	return nil
}
```

每次循环中会检查一次 context 是否已终止. 如果未终止会执行 `dostep`. 即使在 `dostep` 过程中
context 已结束, 也要等到 `dostep` 完成后下次再询问 `ctx.Done`.
如果 `dostep` 会耗时很久, 那么 `context.WithTimeout` 有可能不会取得很好的效果.


3. 是否处理 context 通知由 goroutine 自己决定.

接收 context 通知的 goroutine 完全可以忽略 context 通知. 如果是协作开
发的话, 最好做个约定.

```go
func foo(ctx context.Context) {
	// ignore context notification
	dosomething()
}
```

如果不幸协作者或第三方提供的调用不接受或不处理 context, 那么需要做一层封装.

```go
func foo(ctx context.Context) error {
	finished := make(chan struct{}, 1)

	go func() {
		dosomething()
		finished <- struct{}{}
	}()

	select {
	case <-ctx.Done():
		// to wait for dosomething() to finish, you can
		// <-finished
		return ctx.Err()
	case <-finished:
		return nil
	}
}
```

这里的问题在于, 如果不调用注释里的 `<-finished` 等待 `dosomething` 结
束, 那么 `finished` 必须是一个缓冲信道以避免 goroutine leak. 而无论哪
种情况, `dosomething` 都会执行到结束, 并不会受到 context cancel 的影响.
也就是说, 如果 goroutine 自身不处理 context, 其实没有什么好的办法弥补.
