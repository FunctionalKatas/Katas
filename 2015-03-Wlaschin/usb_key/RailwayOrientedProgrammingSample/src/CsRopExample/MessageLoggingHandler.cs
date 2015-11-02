﻿using System;
using System.Diagnostics;
using System.Net.Http;
using System.Threading;

namespace CsRopExample
{
    /// <summary>
    /// Logging code 
    /// </summary>
    public class MessageLoggingHandler : MessageProcessingHandler
    {

        protected override HttpRequestMessage ProcessRequest(HttpRequestMessage request, CancellationToken cancellationToken)
        {
            var correlationId = string.Format("{0}{1}", DateTime.Now.Ticks, Thread.CurrentThread.ManagedThreadId);
            var requestInfo = string.Format("{0} {1}", request.Method, request.RequestUri);
            var message = request.Content.ReadAsStringAsync().Result;
            Debug.WriteLine("[HTTP]Request: {1}\r\n[HTTP]{2}\r\n\r\n", correlationId, requestInfo, message);
            return request;
        }

        protected override HttpResponseMessage ProcessResponse(HttpResponseMessage response, CancellationToken cancellationToken)
        {
            var correlationId = string.Format("{0}{1}", DateTime.Now.Ticks, Thread.CurrentThread.ManagedThreadId);
            var requestInfo = string.Format("{0} {1}", response.RequestMessage.Method, response.RequestMessage.RequestUri);
            var message = response.Content != null 
                ? response.Content.ReadAsStringAsync().Result 
                : "[no body]";
            Debug.WriteLine("[HTTP]Response: {1}\r\n[HTTP]{2}\r\n\r\n", correlationId, requestInfo, message);
            return response;
        }
    }
}