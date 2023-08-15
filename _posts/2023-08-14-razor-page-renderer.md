---
layout: post
title: 利用 ASP.NET Core Razor Page 生成 html
categories: C#
---

# 利用 ASP.NET Core Razor Page 生成 html

最近参与的一个 ASP.NET web app 需要发送 html 邮件. 直接拼装 html 实在过于痛苦, 于是自然地想到从 html template 来生成 html.
ASP.NET MVC 使用 Razor Page 作为 html template engine. 那如何用 Razor Page 来生成 html 呢?


## ASP.NET Core MVC 是如何生成 html 的呢?

### 探究 MVC View

一开始我们还无从下手, 可以先来看看 ASP.NET MVC 怎么来生成 html. 创建一个 ASP.NET Web App, 

```sh
dotnet new webapp --name RazorPageTest
```

`Pages` 里面生成了预定义好的 Razor Pages. `Pages/Index.cshtml` 长这样:

```html
@page
@model IndexModel
@{
    ViewData["Title"] = "Home page";
}

<div class="text-center">
    <h1 class="display-4">Welcome</h1>
    <p>Learn about <a href="https://docs.microsoft.com/aspnet/core">building Web apps with ASP.NET Core</a>.</p>
</div>
```

`@page` 会让 Razor Page 直接生成 route path 而忽略任何 controller, 这不是我们想要的. 先把它去掉,
让 `Index.cshtml` 变成一个普通的 [Razor View File](https://learn.microsoft.com/en-us/aspnet/core/tutorials/first-mvc-app/adding-view?view=aspnetcore-7.0&tabs=visual-studio#add-a-view).

写一个 Controller:
```C#
using Microsoft.AspNetCore.Mvc;

namespace RazorPageTest
{
  public class ViewController : Controller
  {
    [HttpGet("/")]
    public IActionResult GetIndex()
    {
      return View("~/Pages/Index.cshtml");
    }
  }
}
```

这个时候编译然后启动 RazorPageTest, 我们会在自动打开的页面 (http://localhost:xxxx) 里看到 Index Page. OK, Razor Page 已经变成了一个 html page.

### 嗯? 这个 View 到底做了啥?

什么都还没做呢, 怎么就结束了呢?  `View` 做了啥? 开源世界的好处就是可以去代码里翻出这些黑魔法. 从 `View` 的函数签名我们可以看到它返回了一个 `ViewResult`.

```C#
public abstract class Controller : ControllerBase, IActionFilter, IFilterMetadata, IAsyncActionFilter, IDisposable
{
    // ...
    public virtual ViewResult View(string? viewName);
    // ...
}
```

这个 `ViewResult` 只有一个 method:

```C#
public class ViewResult : ActionResult, IStatusCodeActionResult {
    public override async Task ExecuteResultAsync(ActionContext context)
    {
        // ...
        var executor = context.HttpContext.RequestServices.GetService<IActionResultExecutor<ViewResult>>();
        // ...
    }
}
```

可以想到, 这个 `IActionResultExecutor` 把 `ViewResult` 变成了最终的 html page.
这时候我们已经可以知道如何来手动执行这个过程了, 如果所有这些 `context`, `HttpContext` 都能恰好存在的话...

那是当然, 如果是在一个 Controller 里面的话.

### 生成 html, 在一个 Controller 中

于是我们得到如下版本.

```C#
using Microsoft.AspNetCore.Mvc;

namespace RazorPageTest
{
  public class ViewController : Controller
  {
    [HttpGet("/")]
    public async Task<string> GetPage()
    {
      var viewResult = View("~/Pages/Index.cshtml");
      await viewResult.ExecuteResultAsync(this.ControllerContext);

      var resp = this.ControllerContext.HttpContext.Response;
      using (StreamReader reader = new StreamReader(resp.Body)) // Exception! resp.Body is not readable.
      {
        return reader.ReadToEnd();
      }
    }
  }
}

```

可是, `resp.Body` 只能写不能读. ChatGPT 给出了答案, 用 `MemoryStream` 暂时替换:

```C#
using Microsoft.AspNetCore.Mvc;

namespace RazorPageTest
{
  public class ViewController : Controller
  {
    [HttpGet("/")]
    public async Task<string> GetPage()
    {
      var originalResponseBody = Response.Body;
      string html = string.Empty;
      using (var memoryStream = new MemoryStream())
      {
          Response.Body = memoryStream;

          var viewResult = View("~/Pages/Index.cshtml");
          await viewResult.ExecuteResultAsync(ControllerContext);

          memoryStream.Seek(0, SeekOrigin.Begin);

          using (var reader = new StreamReader(memoryStream))
          {
              html = reader.ReadToEnd();
          }

          Response.Body = originalResponseBody; // Restore the original response body
      }

      return html;
  }
```

[这里](https://stackoverflow.com/questions/43403941/how-to-read-asp-net-core-response-body) 有更多的讨论.

### 可是这个 Controller 必须要发 http request 才能使用

确实, 这是因为 `ControllerContext` 只有在处理 http request 的时候才能被正确设置.

那么, 只要我们能够正确的创建一个 ControllerContext...

```C#
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Abstractions;
using Microsoft.AspNetCore.Mvc.ModelBinding;
using Microsoft.AspNetCore.Mvc.ViewFeatures;

namespace RazorPageTest
{
  public class RazorPageRenderer
  {
    private readonly IServiceProvider _serviceProvider;

    public RazorPageRenderer(IServiceProvider serviceProvider)
    {
      this._serviceProvider = serviceProvider;
    }

    public async Task<string> RenderAsync<TModel>(string viewPath, TModel model)
    {
      var metadataProvider = new EmptyModelMetadataProvider();
      var modelState = new ModelStateDictionary();
      var viewData = new ViewDataDictionary<TModel>(metadataProvider, modelState)
      {
        Model = model
      };

      var httpContext = new DefaultHttpContext { RequestServices = this._serviceProvider };
      using var memoryStream = new MemoryStream();
      httpContext.Response.Body = memoryStream;
      var actionContext = new ActionContext(
          httpContext,
          new RouteData(),
          new ActionDescriptor(),
          modelState);

      var viewResult = new ViewResult
      {
        ViewName = viewPath,
        ViewData = viewData,
      };

      await viewResult.ExecuteResultAsync(actionContext);
      httpContext.Response.Body.Seek(0, SeekOrigin.Begin);

      using var reader = new StreamReader(memoryStream);
      var html = reader.ReadToEnd();
      return html;
    }
  }
}
```

这里把 `~/Pages/Index.cshtml` 换成了参数 `viewPath`. 又加上了 `model`, 这样就可以给 template 提供数据了.

至此我们已经达到了目标.

## 稍微更直接一点点的方案

到 [microsoft/aspnetcore](https://github.com/dotnet/aspnetcore/tree/main/src/Mvc) 简单搜索就可以找到 [ViewResultExecutor的实现](https://github.com/dotnet/aspnetcore/blob/e74ec45ab62052101648144004444fdc5f7d2df5/src/Mvc/Mvc.ViewFeatures/src/ViewResultExecutor.cs#L35C25-L35C25).

再往下扒一扒, 可以发现 html 生成是由 [RazorView.RenderAsync](https://github.com/dotnet/aspnetcore/blob/c723c70b664d06a307cf33cafc761ecacaa7a0d7/src/Mvc/Mvc.Razor/src/RazorView.cs#L76) 完成的. 所以我们可以直接使用它:

```C#
namespace RazorPageTest
{
  using System;
  using System.IO;
  using System.Threading.Tasks;

  using Microsoft.AspNetCore.Mvc;
  using Microsoft.AspNetCore.Mvc.Abstractions;
  using Microsoft.AspNetCore.Mvc.ModelBinding;
  using Microsoft.AspNetCore.Mvc.Razor;
  using Microsoft.AspNetCore.Mvc.Rendering;
  using Microsoft.AspNetCore.Mvc.ViewFeatures;

  public class RazorPageRenderer
  {
    private readonly IRazorViewEngine razorViewEngine;
    private readonly IServiceProvider serviceProvider;
    private readonly ITempDataDictionaryFactory tempDataFactory;

    public RazorPageRenderer(
      IRazorViewEngine razorViewEngine,
      IServiceProvider serviceProvider,
      ITempDataDictionaryFactory tempDataFactory)
    {
      this.razorViewEngine = razorViewEngine;
      this.serviceProvider = serviceProvider;
      this.tempDataFactory = tempDataFactory;
    }

    public async Task<string> RenderAsync<TModel>(string templateName, TModel model)
    {
      var viewEngineResult = razorViewEngine.GetView(executingFilePath: null, viewPath: templateName, isMainPage: true);

      if (!viewEngineResult.Success)
      {
        throw new ArgumentException($"The view '{templateName}' could not be found.");
      }

      var view = viewEngineResult.View;
      using var output = new StringWriter();
      var metadataProvider = new EmptyModelMetadataProvider();
      var modelState = new ModelStateDictionary();
      var viewData = new ViewDataDictionary<TModel>(metadataProvider, modelState)
      {
        Model = model
      };

      var httpContext = new DefaultHttpContext { RequestServices = serviceProvider };
      var actionContext = new ActionContext(
          httpContext,
          new RouteData(),
          new ActionDescriptor(),
          modelState);

      var viewContext = new ViewContext(
        actionContext,
        view, viewData, this.tempDataFactory.GetTempData(httpContext), output, new HtmlHelperOptions { });

      await view.RenderAsync(viewContext);

      return output.ToString();
    }
  }
}  
```

需要提一下, 以上 contructor 中的依赖, 需要用 `AddRazorPages` 或者 `AddControllerWithViews` 来注入. 

```C#
var builder = WebApplication.CreateBuilder(args);
builder.Services.AddRazorPages();
```

## ASP.NET 之外的解决方案

C# 其实也有不少开源的 html template engine. Github [#template-engine](https://github.com/topics/template-engine?l=c%23) 就有 59 个.
直接支持 Razor Page 的也有 [RazorLight](https://github.com/toddams/RazorLight), [RazorCoreEngine](https://github.com/adoconnection/RazorEngineCore).

我相信, 对于通常的应用场景, 这些项目肯定足以胜任. 不过, 无论是开发速度, 还是受信任程度, 他们显然无法与 ASP.NET 的官方实现相比.
这恐怕是在 Microsoft 这个巨无霸的阴影之下的开源项目的尴尬之处.
