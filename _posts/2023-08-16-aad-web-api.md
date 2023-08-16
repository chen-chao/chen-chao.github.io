---
layout: post
title: 用 Azure AD 来验证对 Web API 的访问
categories: C#
---

# 用 Azure AD 来验证对 Web API 的访问

授权访问 Web API 是非常常见的需求. 如果使用 Azure App Service 的话, 最便捷的方式就是使用 Azure AD.

## 为 Web API Service 创建 Azure AD App

官方的文档在[这里](https://learn.microsoft.com/en-us/azure/active-directory/develop/quickstart-configure-app-expose-web-apis).

在 [Azure Portal](https://portal.azure.com) 的 Azure Active Directory 中创建一个 App, 就称为 `Super Secret Service` 吧. 这个 APP 就作为 OAuth2 验证的 Resource App.

在 `Expose an API` 中, 设置 `Application ID Uri`, 默认是 `api://<app-id>` 的形式. 然后添加一个 Scope. 没错, 这个就是你想的那个 Scope, JWT 中的 `scp` 字段. 我们增加了一个 scope 叫 `My.Precious`. 

OK, resouce app 的设置已经完成.

## 创建 Client App

我们要再创建一个 Azure AD App, 用来访问刚刚创建的 Resouce App. 没错, 这个就是 OAuth2 里的那个 Client ID.

通常这个 Client App 和 Resouce App 由不同的团队来开发, 所以它们会有不同的 App ID.
但这里我们可以偷个懒, 可以把刚刚那个 Resource App 作为 Client App. 也没有谁规定不能自己访问自己的嘛.

其他的设置项还是不能少, 在 `API Permissions` 里面添加刚刚创建的 scope, `api://<app-id>/My.Precious`. 在 `Authentication` 里设置好 redirect Uri. 

如果是 Web App, 需要再去 `Certificate & Secrets` 去创建一个 client secret.

## 愉快地开发 Web API

如果用的 Visual Studio, 创建项目的时候选择 ASP.NET Core Web API. 然后在 `Connected Services` 里添加 `Microsoft Identity Platform`, 基本就大功告成了.

剩下的工作就是给需要授权访问的 controller 和 actions 加上相应的 attribute. 比如自带的示例:

```C#
  [RequiredScope(RequiredScopesConfigurationKey = "AzureAd:Scopes")]
  [Authorize]
  [ApiController]
  [Route("[controller]")]
  public class WeatherForecastController : ControllerBase {
    // ...
  }
```

如果不用 Visual Studio... 那我觉得还是赶紧来感受一下 VS 无微不至的照料.

如果你像我一样从不愿意测试代码, 那么到这里开发就已经结束了.

## 被打回做测试

要再写一堆代码去访问 web api, 实在太麻烦了... 好在我们已经有浏览器了.

让我们用正宗的 OAuth2 [authorization code flow](https://learn.microsoft.com/en-us/azure/active-directory/develop/v2-oauth2-auth-code-flow) 去获得 access token.

直接在浏览器中输入 (因为需要 run JS)

```
https://login.microsoftonline.com/<tenant-id>/oauth2/v2.0/authorize?
client_id=<client-id>
&response_type=code
&redirect_uri=<redirect-uri>
&response_mode=query
&scope=openid%20offline_access%20<full-scope>
&state=12345
&code_challenge=<code-challenge>
&code_challenge_method=S256
```
其中, `<tenant-id>` 可以在 Azure Portal 的 Azure AD 界面里找到. `<client-id>`, `<redirect-uri>`, `<full-scope>` 换成前述的创建的值.
`<code-challenge>` 在 [Online PKCE Generator Tool](https://tonyxu-io.github.io/pkce-generator/) 上生成一个就好了. 记下使用的 `<code-verifier>`. 注意这里的字符都要做 url escape.

打开 Developer Tools 查看跳转到 `<redirect-uri>` 的记录, 这里能够找到 authorization code. Redirect uri 能否正常响应无所谓.
大概长这样:

```
GET	http://<redirect-uri>/?code=0.ARoAv4j5cvGGr0GRqy180BHbR_jirWkEdKpKv9sj0hFG...
```

然后就可以用这个 code 去兑换 access token 和 refresh token:

```sh
curl -X POST -H 'Content-Type: application/x-www-form-urlencoded' -d 'client_id=<client-id>&scope=<full-scope>&code=<code>&redirect_uri=<redirect-uri>&grant_type=authorization_code&code_verifier=<code-verifier>&client_secret=<client-secret>' 'https://login.microsoftonline.com/<tenant-id>/oauth2/v2.0/token'
```

返回长这样:

```json
{
  "token_type":"Bearer",
  "scope":"<full-scope>",
  "expires_in":4405,
  "ext_expires_in":4405,
  "access_token": "...",
  "refresh_token": "..."
}
```

拿着这个 access token 去访问 web api 就可以了:

```sh
curl -H 'Authorization: Bearer <access-token>' https://web-api-url
```