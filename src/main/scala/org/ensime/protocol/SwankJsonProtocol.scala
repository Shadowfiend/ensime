package org.ensime.protocol

import java.io._
import org.ensime.config.{ ProjectConfig, DebugConfig, ReplConfig }
import org.ensime.debug.{ DebugUnit, DebugSourceLinePairs }
import org.ensime.model._
import org.ensime.server._
import org.ensime.util._
import org.ensime.util.SExp._
import scala.actors._
import scala.tools.nsc.util.{ Position }
import scala.tools.refactoring.common.Change
import scala.util.parsing.input
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST._
import net.liftweb.json.{Printer, JsonParser}
import scala.util.parsing.input.CharSequenceReader;
import scala.collection.mutable.HashMap;


object SwankJsonProtocol extends SwankJsonProtocol {}

/* note: Vim can't read true/false, neither does it know about "null"
 * I suggest using 1/0 and 0 instead. There are workarounds though
 * the result always contains etiher { 'ok': reply } or { 'error': msg }
 */
trait SwankJsonProtocol extends Protocol[JValue] {
  import SwankProtocol._
  import ProtocolConst._

  val PROTOCOL_VERSION: String = "json-0.0.1"

  val SERVER_NAME: String = "ENSIMEserver"

  private var outPeer: Actor = null;
  private var rpcTarget: RPCTarget = null;

  def peer = outPeer

  def setOutputActor(peer: Actor) { outPeer = peer }

  def setRPCTarget(target: RPCTarget) { this.rpcTarget = target }

  // Handle reading / writing of messages

  def writeMessage(value: JValue, out: Writer) {
    out.write(compact(render(value))+"\n")
    out.flush()
  }

  private def fillArray(in: java.io.Reader, a: Array[Char]) {
    var n = 0
    var l = a.length
    var charsRead = 0;
    while (n < l) {
      charsRead = in.read(a, n, l - n)
      if (charsRead == -1) {
        throw new EOFException("End of file reached in socket reader.");
      } else {
        n += charsRead
      }
    }
  }

  private val headerBuf = new Array[Char](6);

  def readMessage(in: java.io.Reader): JValue = {
      JsonParser.parse(in.asInstanceOf[BufferedReader].readLine)
  }

  def sendBackgroundMessage(code: Int, detail:Option[String]) {
    val msg: JObject = ("background-message" -> ("code" -> code) ~ ("detail" -> detail.getOrElse("")))
    sendMessage(msg)
  }

  def handleIncomingMessage(msg: JValue) {
      handleMessageForm(msg)
  }

  private def handleMessageForm(json: JValue) {

   // this function contains the code of
   // handleMessageForm, handleEmacsRex
   // found in SwankProtocol

    def files(json:JValue) = {
      json match {
        case JArray(l) => l.values.asInstanceOf[List[String]]
        case _ => throw new Exception("filenames expected, got: "+json) // TODO return as error?
      }
    }

    def prepareParams(o:JObject) = {
      val map:Map[String, JValue] = o.values.asInstanceOf[Map[String, JValue]]
      val m = new HashMap[Symbol, Any]()
      for (k <- map.keys){
        m += Symbol(k) -> map.get(k).values
      }
      m.toMap
    }

    println("\nHandling RPC: " + json)

    json match {
      case JArray(JInt(callIdBig) :: JString(rpcname) :: rest) => {

        val callId = callIdBig.toInt

        (rpcname, rest) match {

          case ("swank:connection-info",_) => sendConnectionInfo(callId)
          case ("swank:init-project", JString(type_) :: JString(value) :: jRootDir /* null or str */ :: rest) => {
                val rootDir = jRootDir match {
                  case JString(path) => Some(path)
                  case _ => None
                }
                val config = type_ match {
                  case "json"        => ProjectConfig.fromJson(value.asInstanceOf[JObject], rootDir)
                  case "sexp-string" => ProjectConfig.fromSExp(SExp.read(new CharSequenceReader(value)).asInstanceOf[SExpList], rootDir)
                  case _ => throw new Exception("json or sexp-string expected")
                }
                rpcTarget.rpcInitProject(config, callId)
              }
          case ("swank:peek-undo", _) => rpcTarget.rpcPeekUndo(callId)
          case ("swank:exec-undo", JInt(id)::_) => {
                rpcTarget.rpcExecUndo(id.toInt, callId)
          }
          case ("swank:repl-config", _) => rpcTarget.rpcReplConfig(callId)
          case ("swank:builder-init", _) => rpcTarget.rpcBuilderInit(callId)
          case ("swank:builder-add-files", filenames :: body) => 
                rpcTarget.rpcBuilderAddFiles(files(filenames), callId)
          case ("swank:builder-update-files", filenames :: body) => {
                rpcTarget.rpcBuilderUpdateFiles(files(filenames), callId)
          }
          case ("swank:builder-remove-files", filenames :: body) => {
                rpcTarget.rpcBuilderRemoveFiles(files(filenames), callId)
          }
          case ("swank:debug-config", _) => {
            rpcTarget.rpcDebugConfig(callId)
          }
          case ("swank:debug-unit-info", JString(sourceName) :: JInt(line) :: JString(packPrefix) :: body) => {
                rpcTarget.rpcDebugUnitInfo(sourceName, line.toInt, packPrefix, callId)
          }

          case ("swank:debug-class-locs-to-source-locs", JArray(pairs) :: body) => {
                val nameLinePairs = pairs.flatMap {
                  case JArray(JString(classname) :: JInt(line) :: body) => {
                    Some(classname, line.toInt)
                  }
                  case _ => Some("",  -1)
                }
                rpcTarget.rpcDebugClassLocsToSourceLocs(nameLinePairs, callId)
          }
          case ("swank:typecheck-file", JString(file) :: body) => {
                rpcTarget.rpcTypecheckFile(file, callId)
          }
          case ("swank:typecheck-all", _) => {
            rpcTarget.rpcTypecheckAll(callId)
          }
          case ("swank:scope-completion", JString(file) :: JInt(point) :: JString(prefix) :: JBool(constructor) :: body) => {
                rpcTarget.rpcScopeCompletion(file, point.toInt, prefix, constructor, callId)
          }
          case ("swank:type-completion", JString(file) :: JInt(point) :: JString(prefix) :: boBooleanAtomdy) => {
                rpcTarget.rpcTypeCompletion(file, point.toInt, prefix, callId)
          }
          case ("swank:import-suggestions", JString(file) :: JInt(point) :: JArray(names) :: body) => {
                rpcTarget.rpcImportSuggestions(file, point.toInt,
                  names.map(_.toString).toList, callId)
          }
          case ("swank:package-member-completion", JString(path) :: JString(prefix) :: body) => {
                rpcTarget.rpcPackageMemberCompletion(path, prefix, callId)
          }
          case ("swank:inspect-type-at-point", JString(file) :: JInt(point) :: body) => {
                rpcTarget.rpcInspectTypeAtPoint(file, point.toInt, callId)
          }
          case ("swank:inspect-type-by-id", JInt(id) :: body) => {
                rpcTarget.rpcInspectTypeById(id.toInt, callId)
          }
          case ("swank:symbol-at-point", JString(file) :: JInt(point) :: body) => {
                rpcTarget.rpcSymbolAtPoint(file, point.toInt, callId)
          }
          case ("swank:type-by-id", JInt(id) :: body) => {
                rpcTarget.rpcTypeById(id.toInt, callId)
          }
          case ("swank:type-by-name", JString(name) :: body) => {
                rpcTarget.rpcTypeByName(name, callId)
          }
          case ("swank:type-by-name-at-point", JString(name) :: JString(file) :: JInt(point) :: body) => {
                rpcTarget.rpcTypeByNameAtPoint(name, file, point.toInt, callId)
          }
          case ("swank:call-completion", JInt(id) :: body) => {
                rpcTarget.rpcCallCompletion(id.toInt, callId)
          }
          case ("swank:type-at-point", JString(file) :: JInt(point) :: body) => {
                rpcTarget.rpcTypeAtPoint(file, point.toInt, callId)
          }
          case ("swank:inspect-package-by-path", JString(path) :: body) => {
                rpcTarget.rpcInspectPackageByPath(path, callId)
          }

          case ("swank:perform-refactor", JInt(procId) :: JString(tpe) :: params :: body) => {
                rpcTarget.rpcPerformRefactor(Symbol(tpe), procId.toInt,
                                             prepareParams(params.asInstanceOf[JObject]), callId)
          }

          case ("swank:exec-refactor", JInt(procId) :: JString(tpe) :: body) => {
                rpcTarget.rpcExecRefactor(Symbol(tpe), procId.toInt, callId)
          }

          case ("swank:cancel-refactor", JInt(procId) :: body) => {
                rpcTarget.rpcCancelRefactor(procId.toInt, callId)
          }

          case ("swank:format-source",  filenames :: body) => {
                rpcTarget.rpcFormatFiles(files(filenames), callId)
          }
          case _ => {
            sendRPCError(
              ErrUnrecognizedRPC,
              Some("Unknown :swank-rpc call: " + json.toString),
              callId)
          }
        } // match (rpcname, rest) {
      }
      case _ => {
        sendProtocolError(ErrUnrecognizedForm, Some(json.toString))
      }
    }
  }

  // TODO probably this should be removed for Vim ?
  val emacsCharOffset = 1

  def listOrEmpty(list: JValue): JArray = {
    list match {
      case l: JArray => l
      case _ =>
        // due to the nil nature of Emacs listOrEmpty must be used
        // Json has different representations for [] and false Which is both nil in elisp
        throw new Exception("list expected but got "+list)
    }
  }

  def sendRPCAckOK(callId: Int) {
    sendRPCReturn(RPCResultBool(true), callId)
  }

  def toWF(value :RPCResult):JValue = {
    value match {
      case RPCResultNull =>  JInt(0) // Vim can't parse null
      case RPCResultNoteList(nl:NoteList) => toWF(nl)
      case RPCResultNote(note:Note) => throw new Exception("not used")
      case RPCResultNamedTypeMemberInfo(namedTypeMember:NamedTypeMemberInfoLight) => toWF(namedTypeMember)
      case RPCResultSymbolInfoLight(l: SymbolInfoLight) => toWF(l)
      case RPCResultSymbolInfo(l: SymbolInfo) => toWF(l)
      case RPCResultPackageInfo(l: PackageInfo) => toWF(l)
      case RPCResultPackageMemberInfoLight(i: PackageMemberInfoLight) => toWF(i)
      case RPCResultTypeInfo(i: TypeInfo) => toWF(i)
      case RPCResultCallCompletionInfo(i: CallCompletionInfo) => toWF(i)
      case RPCResultIterable(l: Iterable[RPCResult]) => toWF(l.map(toWF(_)))
      case RPCResultImportSuggestions(l: ImportSuggestions) => toWF(l)
      case RPCResultBool(b: Boolean) => toWF(b)
      case RPCResultTypeInspectInfo(t: TypeInspectInfo) => toWF(t)
      case RPCResultRefactorEffect(b: RefactorEffect) => toWF(b)
      case RPCResultRefactorFailure(b: RefactorFailure) => toWF(b)
      case RPCResultRefactorResult(b: RefactorResult) => toWF(b)
      case RPCResultProjectConfig(c: ProjectConfig) => toWF(c)
      case RPCResultUndo(c: Undo) => toWF(c)
      case RPCResultUndoResult(c: UndoResult) => toWF(c)
      case RPCResultReplConfig(c: ReplConfig) => toWF(c)
      case RPCResultDebugConfig(c: DebugConfig) => toWF(c)
      case RPCResultString(c: String) => toWF(c)
      case RPCResultDebugUnit(d: DebugUnit) => toWF(d)
      case RPCResultDebugSourceLinePairs(p: DebugSourceLinePairs) => toWF(p)
      case RPCResultConnectionInfo =>
            ("pid" -> toWF(null)) ~
            ("server-implementation" -> (":name" -> SERVER_NAME)) ~
            ("version" -> PROTOCOL_VERSION)
    }
  }

  def sendRPCReturn(value: RPCResult, callId: Int) {
    sendMessage(
      ("ok" -> toWF(value)) ~
      ("callId" -> JInt(callId))
    )
  }


  def sendRPCError(code: Int, detail: Option[String], callId: Int) {
    sendMessage(
      ("callId" -> JInt(callId)) ~
      ("error" -> JString(detail.getOrElse("")))
    )
  }

  def sendProtocolError(code: Int, detail: Option[String]){
    sendMessage(
      ("error" -> ("protocol error: " + JString(detail.getOrElse(""))))
    )
  }

  /*
  * A sexp describing the server configuration, per the Swank standard.
  */
  def sendConnectionInfo(callId: Int) = {
    sendRPCReturn(RPCResultConnectionInfo, callId)
  }

  def sendCompilerReady() = sendMessage(JObject(List(JField("compiler-ready", JInt(1)))))

  def sendTypeCheckResult(notelist: NoteList) = {
    sendMessage(JObject(List(JField("typecheck-result", toWF(notelist)))))
  }

  object JValueConversion {

    implicit def posToJValue(pos: Position): JValue = {
      if (pos.isDefined) {
        ("file" -> pos.source.path) ~ ("offset" -> (pos.point + emacsCharOffset))
      } else {
        JNull
      }
    }

  }

  import JValueConversion._

  def toWF(config: ProjectConfig): JValue = {
    ("project-name" -> config.name) ~ ("source-roots" -> config.sourceRoots.map(_.getPath).toList)
  }

  def toWF(config: ReplConfig): JValue = {
    ("classpath" -> config.classpath)
  }

  def toWF(config: DebugConfig): JValue = {
    ("classpath" -> config.classpath) ~ ("sourcepath" -> config.sourcepath)
  }

  def toWF(unit: DebugUnit): JValue = {
    ("full-name" -> unit.classQualName) ~
      ("package" -> unit.packageName) ~
      ("start-line" -> unit.startLine) ~
      ("end-line" -> unit.endLine)
  }

  def toWF(value: Null): JValue = {
    JInt(0)
  }

  def toWF(value: Boolean): JValue = {
    JInt(if (value) 1 else 0)
  }

  def toWF(value: String): JValue = {
    JString(value)
  }


  def toWF(value: DebugSourceLinePairs): JValue = {
    
    JArray(value.pairs.map( p => JObject(List(JField(p._1, p._2)))).toList)
  }

  def toWF(note: Note): JValue = {
    ("severity" -> note.friendlySeverity) ~
    ("msg" -> note.msg) ~
    ("beg" -> (note.beg + emacsCharOffset)) ~
    ("end" -> (note.end + emacsCharOffset)) ~
    ("line" -> note.line) ~
    ("col" -> note.col) ~
    ("file" -> note.file)
  }

  def toWF(notelist: NoteList): JValue = {
    val NoteList(isFull, notes) = notelist
    ("is-full" -> toWF(isFull)) ~ ("notes" -> notes.map(toWF).toList)
  }

  def toWF(values: Iterable[JValue]): JValue = {
    JArray(values.toList.map(_.asInstanceOf[JValue]))
  }

  def toWF(value: SymbolInfoLight): JValue = {
    ("name" -> value.name) ~
    ("type-sig" -> value.tpeSig) ~
    ("type-id" -> value.tpeId) ~
    ("is-callable" -> toWF(value.isCallable))
  }

  def toWF(value: PackageMemberInfoLight): JValue = {
    ("name" -> value.name)
  }

  def toWF(value: SymbolInfo): JValue = {
    ("name" -> value.name) ~
    ("type" -> toWF(value.tpe)) ~
    ("decl-pos" -> value.declPos) ~
    ("is-callable" -> toWF(value.isCallable))
  }

  def toWF(value: NamedTypeMemberInfoLight): JValue = {
    ("name" -> value.name) ~
    ("type-sig" -> value.tpeSig) ~
    ("type-id" -> value.tpeId) ~
    ("is-callable" -> toWF(value.isCallable))
  }

  def toWF(value: NamedTypeMemberInfo): JValue = {
    ("name" -> value.name) ~
    ("type" -> toWF(value.tpe)) ~
    ("pos" -> value.pos) ~
    ("decl-as" -> value.declaredAs)
  }

  def toWF(value: EntityInfo): JValue = {
    value match {
      case value: PackageInfo => toWF(value)
      case value: TypeInfo => toWF(value)
      case value: NamedTypeMemberInfo => toWF(value)
      case value: NamedTypeMemberInfoLight => toWF(value)
      case value => throw new IllegalStateException("Unknown EntityInfo: " + value)
    }
  }

  def toWF(value: TypeInfo): JValue = {
    value match {
      case value: ArrowTypeInfo =>
      {
        ("name" -> value.name) ~
        ("type-id" -> value.id) ~
        ("arrow-type" -> toWF(true)) ~
        ("result-type" -> toWF(value.resultType)) ~
        ("param-sections" -> JArray(value.paramSections.map(toWF).toList))
      }
      case value: TypeInfo =>
      {
        ("name" -> value.name) ~
        ("type-id" -> value.id) ~
        ("full-name" -> value.fullName) ~
        ("decl-as" -> value.declaredAs) ~
        ("type-args" -> value.args.map(toWF).toList) ~
        ("members" -> value.members.map(toWF).toList) ~
        ("pos" -> value.pos) ~
        ("outer-type-id" -> value.outerTypeId.map(x => JInt(x)).getOrElse(JNull))
      }
      case value => throw new IllegalStateException("Unknown TypeInfo: " + value)
    }
  }

  def toWF(value: PackageInfo): JValue = {
    ("name" -> value.name) ~
    ("info-type" -> "package") ~
    ("full-name" -> JString(value.fullname)) ~
    ("members" -> value.members.map(toWF).toList)
  }

  def toWF(value: CallCompletionInfo): JValue = {
    // this may be broken? (TODO: test)
    ("result-type" -> toWF(value.resultType)) ~
    ("params-sections" -> JArray(value.paramSections.map(toWF).toList))
  }

  def toWF(value: ParamSectionInfo): JValue = {
    ("params" -> JArray(value.params.map {
        case (nm, tp) => JArray(List(toWF(nm), toWF(tp)))
      }.toList)
    ) ~
    ("is-implicit" -> toWF(value.isImplicit))
   }

  def toWF(value: InterfaceInfo): JValue = {
    ("type" -> toWF(value.tpe)) ~ ("via-view" -> value.viaView.getOrElse(""))
  }

  def toWF(value: TypeInspectInfo): JValue = {
    ("type" -> toWF(value.tpe)) ~
    ("info-type" -> "typeInspect") ~
    ("companion-id" -> value.companionId.map(JInt(_)).getOrElse(JNull))
    ("interfaces" -> value.supers.map(toWF).toList)
  }

  def toWF(value: RefactorFailure): JValue = {
    ("procedure-id" -> value.procedureId) ~
    ("status" -> "failure") ~
    ("reason" -> value.message)
  }

  def toWF(value: RefactorEffect): JValue = {
    ("procedure-id" -> value.procedureId) ~
    ("refactor-type" -> value.refactorType) ~
    ("status" -> "success") ~
    ("changes" -> value.changes.map(changeToWF).toList)
  }

  def toWF(value: RefactorResult): JValue = {
    ("procedure-id" -> value.procedureId) ~
    ("refactor-type" -> value.refactorType) ~
    ("touched-files" -> value.touched.map(_.getAbsolutePath).toList)
  }

  def toWF(value: ImportSuggestions): JValue = {
    JArray(value.symLists.map(l => JArray(l.map(toWF).toList)).toList)
  }

  def toWF(value: Undo): JValue = {
    ("id" -> value.id) ~ ("changes" -> value.changes.map(changeToWF).toList) ~ ("summary" -> value.summary)
  }

  def toWF(value: UndoResult): JValue = {
    ("id" -> value.id) ~ ("touched-files" -> value.touched.map(_.toString).toList)
  }

  private def changeToWF(ch: Change): JValue = {
    ("file" -> ch.file.path) ~
      ("text" -> ch.text) ~
      ("from" -> emacsCharOffset) ~
      ("to" -> (ch.to + emacsCharOffset))
  }

}

